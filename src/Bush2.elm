module Bush2 exposing (..)
import Angle
import Camera3d
import Length exposing (Meters, meters)
import Color
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Length
import Pixels
import Point3d exposing (Point3d)
import Sphere3d
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
                   ,prev: Maybe{x:Float,y:Float}}
type Msg = Elapsed Time.Posix
    | PDown {x:Float,y:Float}
    | PMove {x:Float,y:Float}
    | PUp
type alias State coordinates  = { pos: Point3d Meters coordinates
                                , dir: Vector3d Meters coordinates
                                , normDir: Vector3d Meters coordinates
                                , level: Int
                                }

init: () -> (Model, Cmd Msg)
init _ =
    let
        dummy=(treeView3d (Lsystem3d.iterateGrow "A" 1)
                                   (State Point3d.origin
                                        (Vector3d.meters 0 0 1)
                                        (Vector3d.meters 0 2 0)
                                        0
                                   )
                                   []
                                   (Material.nonmetal
                { baseColor = Color.lightBrown
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                })
                              )
    in
    (Model(Debug.log "" <| Lsystem3d.iterateGrow "A" 6)
    (Point3d.meters 0 20 30)
        Nothing
    ,Cmd.none)

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
                              {x=-pos.y+ppos.y
                              ,y=pos.x-ppos.x}
                          Nothing ->
                              {x=1
                              ,y=0}
                axis = Maybe.withDefault Axis3d.x <|
                       Axis3d.throughPoints
                           Point3d.origin
                               (Point3d.meters dir.x 0 dir.y)
                angle = Angle.degrees (sqrt (dir.x^2 + dir.y^2))
                eyePoint = case model.prev of
                               Just q -> Point3d.rotateAround axis angle model.eyePoint
                               Nothing -> model.eyePoint
                dummy= pos
            in
                 ({model|prev=case model.prev of
                                  Just q ->Just pos
                                  Nothing -> Nothing
                  ,eyePoint = eyePoint}, Cmd.none)
        PUp ->
            ({model|prev=Nothing}, Cmd.none)
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

        plane =
          Scene3d.quad materialGreen
              (Point3d.meters 100 100 0)
              (Point3d.meters -100 100 0)
              (Point3d.meters -100 -100 0)
              (Point3d.meters 100 -100 0)

                    
        -- Define a camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = (Point3d.meters 0 0 0)
                        , eyePoint = model.eyePoint
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        relativePos event =
            {x=Tuple.first event.pointer.offsetPos
            ,y=Tuple.second event.pointer.offsetPos}
    in
    Html.div[Pointer.onDown (\event -> PDown (relativePos event))
            ,Pointer.onMove (\event -> PMove (relativePos event))
            ,Pointer.onUp (\event -> PUp)]
        [
        Scene3d.sunny
          { camera = camera
          , clipDepth = Length.centimeters 0.5
          , dimensions = ( Pixels.int 1000, Pixels.int 1000 )
          , background = Scene3d.transparentBackground
          , entities = [plane]++  (treeView3d model.word
                                   (State Point3d.origin
                                        (Vector3d.meters 0 0 1)
                                        (Vector3d.meters 0 2 0)
                                        0
                                   )
                                   []
                                   material
                              )
          , shadows = True

          , upDirection = Direction3d.z

          , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        }
        ]


treeView3d: String -> State coordinates -> List (State  coordinates) -> Material.Uniform coordinates -> List (Scene3d.Entity coordinates)
treeView3d str state stack material =
    if (String.length str) == 0 then
        []
    else
        let
            head = String.left 1 str
            tail = String.dropLeft 1 str
            unitLength = 0.5
            unitLine =
                Scene3d.cylinderWithShadow material <|
                    Cylinder3d.startingAt 
                        state.pos (Maybe.withDefault Direction3d.x <| Vector3d.direction state.dir)
                            {radius = Length.meters (0.1*(0.8^toFloat(state.level)))
                            ,length = Length.meters (1*unitLength)
                            }
            materialLeaf =
                Material.nonmetal
                    { baseColor = Color.rgb 1 0.8 0.8
                    , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                    }
            leaf = Scene3d.sphereWithShadow materialLeaf (Sphere3d.atPoint state.pos (Length.meters 0.1))
            dir = Vector3d.rotateAround
                  (Axis3d.through state.pos
                       (Maybe.withDefault Direction3d.x <| Vector3d.direction state.normDir))
                  (Angle.degrees 22.5) 
                      state.dir
            normDir = Vector3d.rotateAround
                      (Axis3d.through state.pos
                           (Maybe.withDefault Direction3d.x <| Vector3d.direction state.dir))
                      (Angle.degrees 22.5) 
                          state.normDir

        in
            case head of
                "F" -> unitLine :: (treeView3d tail
                                        {state | 
                                         pos = Point3d.translateBy
                                               (Vector3d.scaleTo (Length.meters unitLength) state.dir)
                                               state.pos
                                        }
                                        stack material)
                "L" -> leaf :: (treeView3d tail
                                    state
                                    stack
                                    material
                               )
                "&" ->  (treeView3d tail
                                        {state |
                                         dir = dir
                                        }
                                        stack material)

                "/" -> (treeView3d tail
                                        {state |
                                         normDir = normDir
                                        }
                                        stack material)
                       
                "[" ->
                    treeView3d tail  {state | level= state.level+1 } (state::stack) material
                "]" ->
                    let
                        popped = Debug.log "pop" <| Maybe.withDefault state (List.head stack)
                        
                                      
                    in
                        treeView3d tail popped
                                       (List.drop 1 stack) material
                _ -> treeView3d tail state stack material

                     
        
subscriptions: Model -> Sub Msg
subscriptions model =
      Time.every 10 Elapsed
