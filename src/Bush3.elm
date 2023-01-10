module Bush3 exposing (..)

import Random
import Angle
import Camera3d
import Length exposing (Meters, meters)
import Color
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Html.Attributes as Atts
import Html.Events as Events
import Length
import Pixels
import Point3d exposing (Point3d)
import Sphere3d
import Triangle3d
import Cylinder3d
import Axis3d
import Vector3d exposing (Vector3d)
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d
import Browser
import Html exposing (Html)
import Time
import Lsystem3d
import Html.Events.Extra.Pointer as Pointer

main = Browser.element {init = init
                        ,update = update
                        ,view = view
                        ,subscriptions = subscriptions}

type WorldCoordinates = WorldCoordinates

type alias Model = {word: String
                   ,eyePoint: Point3d.Point3d Length.Meters WorldCoordinates
                   ,prev: Maybe{x:Float,y:Float}
                   ,grass: List {x:Float, y:Float, theta:Float}
                   ,angle: Float
                   ,leaf: Leaf
                   }
type Leaf = Leaf
          | None
          | Flower
    
type Msg = Elapsed Time.Posix
    | PDown {x:Float,y:Float}
    | PMove {x:Float,y:Float}
    | PUp
    | Zoom
    | GrassPoints (List {x:Float, y:Float, theta:Float})
    | IncAngle 
    | DecAngle
    | LeafShape Leaf
    | Grow
      
type alias State coordinates  = { pos: Point3d Meters coordinates
                                , dir: Vector3d Meters coordinates
                                , normDir: Vector3d Meters coordinates
                                , level: Int
                                }

init: () -> (Model, Cmd Msg)
init _ =
    (Model (Lsystem3d.iterateGrow "A" 3)
         (Point3d.meters 0 30 15)
         Nothing
         []
         22.5
         Leaf
    ,Random.generate GrassPoints
        (Random.list 5000 (Random.map3 (\x y theta -> {x=x, y=y, theta=theta})
                               (Random.float -10 10) (Random.float -10 10) (Random.float 0 (2*pi)))
        )
    )

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PDown pos ->
            let
                dummy= pos
            in
                 ({model|prev=Just pos}, Cmd.none)
        PMove pos ->
            let
                dir = case model.prev of
                          Just ppos ->
                              {x=pos.x-ppos.x
                              ,y=pos.y-ppos.y}
                          Nothing ->
                              {x=1
                              ,y=0}
                              
                theta = Debug.log "theta" <| if dir.x^2 + dir.y^2 > 0 then
                                                 asin (dir.x/((sqrt (dir.x^2 + dir.y^2))))
                                             else
                                                 0
                coor = Point3d.toRecord Length.inMeters model.eyePoint
                axis = Maybe.withDefault Axis3d.x <|
                       Axis3d.throughPoints
                           Point3d.origin
                               (Point3d.meters (-coor.y*(cos theta))
                                    (coor.x*(cos theta))
                                    ((sqrt (coor.x^2 + coor.y^2))*(sin theta))
                               )
                angle = Angle.degrees (if dir.y > 0 then
                                           ((sqrt (dir.x^2 + dir.y^2))*0.1)
                                       else
                                           (-(sqrt (dir.x^2 + dir.y^2))*0.1)
                                      )
                eyePoint = case model.prev of
                               Just q -> Point3d.rotateAround axis angle model.eyePoint
                               Nothing -> model.eyePoint
                dummy= Debug.log "ppos" model.prev
                dummy2 = Debug.log "pos" pos
            in
                 ({model|prev=case model.prev of
                                  Just q ->Just pos
                                  Nothing -> Nothing
                  ,eyePoint = eyePoint}, Cmd.none)
        PUp ->
            ({model|prev=Nothing}, Cmd.none)
        Zoom ->
            let
                coor = Point3d.toRecord Length.inMeters model.eyePoint
                dist = sqrt (coor.x^2 + coor.y^2 + coor.z^2)
                eyePoint = Point3d.meters (0.9*coor.x) (0.9*coor.y) (0.9*coor.z)
            in
                ({model|eyePoint = eyePoint}, Cmd.none)
        GrassPoints list ->
            ({model | grass = list}, Cmd.none)
        IncAngle ->
            ({model | angle = model.angle+1}, Cmd.none)
        DecAngle ->
            ({model | angle = model.angle-1}, Cmd.none)
        LeafShape shape ->
            ({model | leaf = shape}, Cmd.none)
        Grow ->
            ({model | word = Lsystem3d.grow model.word}
            ,Cmd.none
            )
        _ -> 
            (model, Cmd.none)
        

view: Model -> Html Msg
view model =
   let
        material =
            Material.nonmetal
                { baseColor = Color.lightBrown
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        materialGreen =
            Material.nonmetal
                { baseColor = Color.green
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }
        materialDart =
            Material.nonmetal
                { baseColor = Color.rgba 0.4 0.6 0.3 0.8
                , roughness = 0.1 -- varies from 0 (mirror-like) to 1 (matte)
                }

        plane =
          Scene3d.quad materialDart
              (Point3d.meters 100 100 0)
              (Point3d.meters -100 100 0)
              (Point3d.meters -100 -100 0)
              (Point3d.meters 100 -100 0)

                    
        -- Define a camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = (Point3d.meters 0 0 1.5)
                        , eyePoint = model.eyePoint
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        relativePos event =
            {x=Tuple.first event.pointer.offsetPos
            ,y=Tuple.second event.pointer.offsetPos}
        treeEntity = (treeView3d model.word
                           (State Point3d.origin
                                (Vector3d.meters 0 0 1)
                                (Vector3d.meters 0 2 0)
                                0
                           )
                           []
                          {material = material
                          ,angle = model.angle
                          ,leaf = model.leaf
                          }
                      )
      {-
        treeEntity2 = List.map (\e -> Scene3d.translateBy
                                      (Vector3d.meters 5 0 0) e
                               )
                      treeEntity
        treeEntity3 = List.map (\e -> Scene3d.translateBy
                                      (Vector3d.meters 5 5 0) e
                               )
                      treeEntity
        treeEntity4 = List.map (\e -> Scene3d.translateBy
                                      (Vector3d.meters 5 -5 0) e
                               )
                      treeEntity
        grassEntity = List.map (\p -> grassView p.x p.y p.theta)
                      model.grass
       -}
    in
    Html.div[Pointer.onDown (\event -> PDown (relativePos event))
            ,Pointer.onMove (\event -> PMove (relativePos event))
            ,Pointer.onUp (\event -> PUp)]
        [Html.button [Events.onClick Zoom] [Html.text "zoom"]
        ,Html.button [Events.onClick IncAngle] [Html.text "+ angle"]
        ,Html.button [Events.onClick DecAngle] [Html.text "- angle"]
        ,Html.button [Events.onClick Grow] [Html.text "grow"]
        ,Html.label []
            [ Html.input
                  [ Atts.type_ "radio"
                  , Atts.name "leaf"
                  , Atts.value "leaf"
                  , Events.onInput (\s -> LeafShape Leaf)
                  ]
                  []
            , Html.text "leaf"
            ]
        ,Html.label []
            [ Html.input
                  [ Atts.type_ "radio"
                  , Atts.name "leaf"
                  , Atts.value "none"
                  , Events.onInput (\s -> LeafShape None)
                  ]
                  []
            , Html.text "none"
            ]
        ,Html.label []
            [ Html.input
                  [ Atts.type_ "radio"
                  , Atts.name "leaf"
                  , Atts.value "flower"
                  , Events.onInput (\s -> LeafShape Flower)
                  ]
                  []
            , Html.text "flower"
            ]
        ,Scene3d.sunny
          { camera = camera
          , clipDepth = Length.centimeters 0.5
          , dimensions = ( Pixels.int 1000, Pixels.int 1000 )
          , background = Scene3d.transparentBackground
          , entities = [plane]  ++ treeEntity  -- ++ treeEntity2 ++ treeEntity3 -- ++ treeEntity4
          , shadows = True
          , upDirection = Direction3d.z
          , sunlightDirection = Direction3d.yz (Angle.degrees -120)
          }
        ]


grassView: Float -> Float -> Float -> Scene3d.Entity coordinates
grassView x y theta =
    let
        materialGreen =
            Material.nonmetal
                { baseColor = Color.darkGreen
                , roughness = 0.4
                }
    in
        
        Scene3d.group <|
            List.map (\g -> Scene3d.rotateAround Axis3d.z (Angle.radians theta) g) <|
                [
                 Scene3d.quad materialGreen
                     (Point3d.meters x y 0)
                     (Point3d.meters (x+0.05) (y+0.05) 0)
                     (Point3d.meters (x+0.05) (y+0.05) 0.1)
                     (Point3d.meters x y 0.1)
                ,Scene3d.quad materialGreen
                    (Point3d.meters (x+0.05) (y+0.05) 0.1)
                    (Point3d.meters x y 0.1)
                    (Point3d.meters x (y-0.1) 0.3)
                    (Point3d.meters (x+0.1) (y) 0.3)
                ,Scene3d.quad materialGreen
                    (Point3d.meters x (y-0.1) 0.3)
                    (Point3d.meters (x+0.1) (y) 0.3)
                    (Point3d.meters (x+0.2) (y) 0.5)
                    (Point3d.meters (x+0.2) (y+0.001) 0.5)
                 {-
            Scene3d.quad materialGreen
                 (Point3d.meters x y 0)
                 (Point3d.meters (x+0.05) (y+0.05) 0)
                 (Point3d.meters (x+0.05) (y+0.05) 0.5)
                 (Point3d.meters x y 0.5)
            ,Scene3d.quad materialGreen
                 (Point3d.meters (x+0.05) (y+0.05) 0.5)
                 (Point3d.meters x y 0.5)
                 (Point3d.meters x (y-0.1) 0.8)
                 (Point3d.meters (x+0.1) (y) 0.8)
            ,Scene3d.quad materialGreen
                 (Point3d.meters x (y-0.1) 0.8)
                 (Point3d.meters (x+0.1) (y) 0.8)
                 (Point3d.meters (x+0.2) (y) 1.2)
                 (Point3d.meters (x+0.2) (y+0.001) 1.2)
                
            ,Scene3d.quad materialGreen
                 (Point3d.meters x y 0)
                 (Point3d.meters (x+0.05) (y+0.05) 0)
                 (Point3d.meters (x+0.05) (y+0.05) 0.9)
                 (Point3d.meters x y 0.9)
            ,Scene3d.quad materialGreen
                 (Point3d.meters (x+0.05) (y+0.05) 0.9)
                 (Point3d.meters x y 0.9)
                 (Point3d.meters x (y-0.1) 1.5)
                 (Point3d.meters (x+0.1) (y) 1.5)
            ,Scene3d.quad materialGreen
                 (Point3d.meters x (y-0.1) 1.5)
                 (Point3d.meters (x+0.1) (y) 1.5)
                 (Point3d.meters (x+0.1) (y) 2)
                 (Point3d.meters (x+0.1) (y+0.001) 2)
                
            ,Scene3d.quad materialGreen
                 (Point3d.meters x y 0)
                 (Point3d.meters (x-0.05) (y-0.05) 0)
                 (Point3d.meters (x-0.05) (y-0.05) 0.5)
                 (Point3d.meters x y 0.5)
            ,Scene3d.quad materialGreen
                 (Point3d.meters (x-0.05) (y-0.05) 0.5)
                 (Point3d.meters x y 0.5)
                 (Point3d.meters x (y+0.1) 1)
                 (Point3d.meters (x-0.1) (y) 1)
            ,Scene3d.quad materialGreen
                 (Point3d.meters x (y+0.1) 1)
                 (Point3d.meters (x-0.1) (y) 1)
                 (Point3d.meters (x-0.2) (y) 1.5)
                 (Point3d.meters (x-0.2) (y-0.001) 1.5)
                 -}
             ]
        
treeView3d: String ->
            State coordinates ->
                List (State  coordinates) ->
                    {material: Material.Uniform coordinates
                    ,angle: Float
                    ,leaf: Leaf
                    }
                    -> List (Scene3d.Entity coordinates)
treeView3d str state stack attrs =
    if (String.length str) == 0 then
        []
    else
        let
            head = String.left 1 str
            tail = String.dropLeft 1 str
            unitLength = 0.5
            unitLine =
                Scene3d.cylinderWithShadow attrs.material <|
                    Cylinder3d.startingAt 
                        state.pos (Maybe.withDefault Direction3d.x <| Vector3d.direction state.dir)
                            {radius = Length.meters (0.1*(0.8^toFloat(state.level)))
                            ,length = Length.meters (1*unitLength)
                            }
            dir = Vector3d.rotateAround
                  (Axis3d.through state.pos
                       (Maybe.withDefault Direction3d.x <| Vector3d.direction state.normDir)
                  )
                  (Angle.degrees attrs.angle) 
                  state.dir
            normDir = Vector3d.rotateAround
                      (Axis3d.through state.pos
                           (Maybe.withDefault Direction3d.x <| Vector3d.direction state.dir)
                      )
                      (Angle.degrees 22.5) 
                      state.normDir
            leafMaterial =
                Material.nonmetal
                    { baseColor = Color.darkGreen -- Color.rgba 0.7 0.9 0.7 0.5
                    , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                    }
            flowerMaterial =
                Material.nonmetal
                    { baseColor = Color.rgba 0.8 0.7 0.7 0.5
                    , roughness = 0 -- varies from 0 (mirror-like) to 1 (matte)
                    }
            comp = Vector3d.toRecord Length.inMeters state.dir
            theta = if comp.x >= 0 then
                        asin (comp.y/(sqrt (comp.x^2 + comp.y^2)))
                    else
                        -(asin (comp.y/(sqrt (comp.x^2 + comp.y^2))))
            leavesView = List.map
                         (\e -> Scene3d.translateBy (Vector3d.from Point3d.origin state.pos) <|
                              Scene3d.rotateAround Axis3d.z (Angle.radians theta) e)
                         [Scene3d.quadWithShadow leafMaterial
                              (Point3d.meters 0.2 0.1 0.05)
                              (Point3d.meters 0.2 0.0 0.05)
                              (Point3d.meters 0.2 -0.1 0.05)
                              (Point3d.meters -0.1 0.0 0)
                         ,Scene3d.quadWithShadow leafMaterial
                             (Point3d.meters 0.2 -0.1 0.05)
                             (Point3d.meters 0.2 0.0 0.05)
                             (Point3d.meters 0.2 0.1 0.05)
                             (Point3d.meters 0.4 0 -0.2)
                                 
                         ,Scene3d.quadWithShadow leafMaterial
                             (Point3d.meters 0.1 0.2 -0.05)
                             (Point3d.meters 0.0 0.2 -0.05)
                             (Point3d.meters -0.1 0.2 -0.05)
                             Point3d.origin
                         ,Scene3d.quadWithShadow leafMaterial
                             (Point3d.meters 0.1 0.2 -0.05)
                             (Point3d.meters 0.0 0.2 -0.05)
                             (Point3d.meters -0.1 0.2 -0.05)
                             (Point3d.meters 0 0.4 -0.1)
                                 
                         ,Scene3d.quadWithShadow leafMaterial
                             (Point3d.meters -0.2 0.1 -0.05)
                             (Point3d.meters -0.2 0.0 -0.05)
                             (Point3d.meters -0.2 -0.1 -0.05)
                            Point3d.origin
                         ,Scene3d.quadWithShadow leafMaterial
                             (Point3d.meters -0.2 -0.1 -0.05)
                             (Point3d.meters -0.2 0.0 -0.05)
                             (Point3d.meters -0.2 0.1 -0.05)
                             (Point3d.meters -0.4 0 -0.2)
                         ]
            flowerView = [Scene3d.translateBy (Vector3d.from Point3d.origin state.pos) <|
                              Scene3d.sphere flowerMaterial <|
                              Sphere3d.atOrigin (Length.meters 0.1)
                         ]
        in
            case head of
                "F" -> unitLine :: (treeView3d tail
                                        {state | 
                                         pos = Point3d.translateBy
                                               (Vector3d.scaleTo (Length.meters unitLength) state.dir)
                                               state.pos
                                        }
                                        stack
                                        attrs
                                   )
                "&" ->  (treeView3d tail
                             {state |
                                  dir = dir
                             }
                             stack
                             attrs
                        )

                "/" -> (treeView3d tail
                            {state |
                                 normDir = normDir
                            }
                            stack
                            attrs
                       )
                       
                "[" ->
                    treeView3d tail  {state | level= state.level+1 } (state::stack) attrs
                "]" ->
                    let
                        popped = Maybe.withDefault state (List.head stack)
                    in
                        treeView3d tail popped
                                       (List.drop 1 stack) attrs
                "L" ->
                    (case attrs.leaf of
                        Leaf -> leavesView
                        Flower -> leavesView ++ flowerView
                        None -> []
                    ) ++(treeView3d tail
                             state
                             stack
                             attrs
                        )
                _ -> treeView3d tail state stack attrs

                     
        
subscriptions: Model -> Sub Msg
subscriptions model =
      Time.every 10 Elapsed
