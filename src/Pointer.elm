module Pointer exposing (..)

import Html exposing (Html)
import Html.Attributes
import Svg exposing (..)
import Svg.Events exposing (..)
import Svg.Attributes exposing (..)
import Html.Events.Extra.Pointer exposing (..)
import Browser
import Time

main = Browser.element{init = init
                      ,update=update
                      ,view=view
                      ,subscriptions=subscriptions}

type alias Piece = {x:Float
                   ,y:Float
                   ,color:Int
                   ,matching:Bool
                   ,moving:Bool}
    
type alias Model = Piece
    
type Msg = Down (Float, Float)
         | Move (Float, Float) 
         | Up (Float, Float)
         | Elapsed Time.Posix


init:()-> (Model, Cmd Msg)
--init _ = (Piece 0 0 0 False False, Cmd.none)
init _ = ({x=0, y=0, color=0, matching=False, moving=False}, Cmd.none)

unit=50
      
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Down (x,y) ->
            Debug.log "down" <| ({model|x=x, y=y, moving=True}, Cmd.none)
        Up (x,y) ->
            let
                xx=toFloat (round ((x-unit/2)/unit))*unit+unit/2
                yy=toFloat (round ((y-unit/2)/unit))*unit+unit/2
            in
                Debug.log "up" <|  ({model|x=xx, y=yy, moving=False}, Cmd.none)
        Move (x,y) ->
            Debug.log "move" <|  (if model.moving then
                                     ({model|x=x, y=y}, Cmd.none)
                                  else
                                      (model, Cmd.none))
        _ -> (model, Cmd.none)
                

pieceView: Piece -> Svg Msg
pieceView piece =
    rect [x (String.fromFloat (piece.x-unit/2))
         ,y (String.fromFloat (piece.y-unit/2))
         ,width "50"
         ,height "50"
         ,fill "pink"
         ,onDown (relativePos >> Down)
         ,onUp (relativePos >> Up)
         ]
        []

frameView: Int -> List (Svg Msg)
frameView size  =
    (
     (List.map (\i -> line [x1 (String.fromInt (i*unit))
                           ,y1 "0"
                           ,x2 (String.fromInt (i*unit))
                           ,y2 (String.fromInt (size*unit))
                           ,stroke "black"
                           ]
                    [])(List.range 0 size))
         ++
         (List.map (\i -> line [x1 "0"
                               ,y1 (String.fromInt (i*unit))
                               ,x2 (String.fromInt (size*unit))
                               ,y2 (String.fromInt (i*unit))
                               ,stroke "black"
                              ]
                       [])
         (List.range 0 size))
    )
            
view: Model -> Html Msg
view model =
    Html.div [Html.Attributes.style "touch-action" "none"
             ]
        [
         svg [width "800"
             ,height "800"
             ,onMove (relativePos >> Move)
             ]
             ([pieceView model]++ (frameView 5))
        ]

relativePos : Event -> ( Float, Float )
relativePos event =
    event.pointer.offsetPos            
            
subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 100 Elapsed
