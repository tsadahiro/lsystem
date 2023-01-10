module Lsystem3d exposing (..)

import Browser
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html.Events

main = Browser.element {init = init
                        ,update = update
                        ,view = view
                        ,subscriptions = subscriptions}

type alias State = {x:Float, y:Float, theta:Float, level:Int}
type alias Model = {word:String, level:Int}
type Msg = Grow

init: () -> (Model, Cmd Msg)
init _ =
    --(Model "0" 0, Cmd.none)
    (Model (iterateGrow "A" 0) 0, Cmd.none)

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Grow -> ({model | level = model.level +1
                      , word = grow model.word}
                ,Cmd.none)

grow: String -> String
grow word =
    String.concat (
        List.map (\letter -> case letter of
                                 'A' -> "[&FL!A]/////’[&FL!A]///////’[&FL!A]"
                                 --'A' -> "[&F!A]/////’[&F!A]///////’[&F!A]"
                                 'F' -> "S/////F"
                                 --'S' -> "FL"
                                 'S' -> "F"
                                 --'L' -> "[’’’∧∧{-f+f+f-|-f+f+f}]"
                                 _ -> String.fromChar letter
                 )
            (String.toList word))

iterateGrow: String -> Int -> String
iterateGrow word itnum =
    if itnum == 0 then
        word
    else
        iterateGrow (grow word) (itnum-1)
        
        
view: Model -> Html Msg
view model =
    Html.div [][
         svg [width "600"
             ,height "600"
             ]
             (treeView model (State 15 30 (-pi/2) 1) [])
        ,Html.br [][]
        ,Html.button [Html.Events.onClick Grow]
             [text "grow"]
        ,Html.br [][]
        ,Html.text model.word
         ]

unit=2
        
treeView: Model -> State -> List State -> List (Html Msg)
treeView model pos stack =
    let
        str = model.word
    in
        if (String.length str) == 0 then
            []
        else
            let
                head = String.left 1 str
                tail = String.dropLeft 1 str
                unitLine = line [x1 (String.fromFloat (unit*pos.x))
                                ,y1 (String.fromFloat (unit*pos.y))
                                ,x2 (String.fromFloat (unit*(((cos pos.theta))+pos.x)))
                                ,y2 (String.fromFloat (unit*(((sin pos.theta))+pos.y)))
                                ,stroke "black"
                                ][]
            in
                case head of
                    "0" -> unitLine :: (treeView {model|word=tail}
                                            {pos |
                                             x=((cos pos.theta)+pos.x)
                                            ,y=((sin pos.theta)+pos.y)
                                            }
                                            stack)
                    "1" -> unitLine :: (treeView {model|word=tail}
                                            {pos |
                                             x=((cos pos.theta)+pos.x)
                                            ,y=((sin pos.theta)+pos.y)
                                            }
                                            stack)
                    "[" -> treeView {model|word=tail} {pos|theta=pos.theta+(pi/4)} (pos::stack) -- push
                    "]" ->
                        let
                            popped = Maybe.withDefault (State 0 0 0 1) (List.head stack)
                        in
                            treeView {model|word=tail} {popped|theta=popped.theta-(pi/4)} (List.drop 1 stack) -- pop
                    _ -> []


        
subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.none
