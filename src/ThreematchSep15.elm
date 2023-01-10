module ThreematchSep15 exposing(..)

import Svg exposing(..)
import Svg.Attributes exposing(..)
import Svg.Events exposing(..)
import Browser
import Array
import Random
import Time
import Html
import File.Download

main = Browser.element {init = init, update = update, view = view, subscriptions = subscriptions}

type alias Piece = {x:Int
                   ,y:Int
                   ,color:Int
                   ,matching:Bool
                   }
       
type State = Waiting
           | Moving Piece
           | Matching
           | Deleting
           | Dropping
           | Adding
       
type alias Game = {conf:List Piece
                   ,state : State
                   ,elapsed: Int
                   ,simuNum:Int
                   ,simuResults: List {x:Int, y:Int, dir:Int, id:Int, size:Int}
                  }
type Model = Setup
    | Playing Game
    | Simulating Game
    | Summary Game
    
type Msg = Roll
         | Generated (List Int)
         | MoveStart Piece
         | MoveEnd Piece
         | Elapsed Time.Posix
         | Added (List Int)
         | Started
         | SimulationStarted
         | RandSwap Int
         | Quited 

size=4
numColor = 3
              
init: () -> (Model, Cmd Msg)
init _ = (Setup, Cmd.none)

downLoad: String -> Cmd msg
downLoad result =
    File.Download.string "result.csv" "text/csv" result
         
row y =
    List.map (\x -> {x=x, y=y, color=0, matching = False}) (List.range 0 (size-1))

hMatchAt: Piece -> Game -> List Piece
hMatchAt p game =
    let
        right = List.filter (\q -> q.color == p.color &&
                         q.y == p.y &&
                         q.x - p.x <= 2 &&
                         q.x >= p.x ) game.conf
        others = List.filter (\q -> not(q.color == p.color &&
                         q.y == p.y &&
                         q.x - p.x <= 2 &&
                         q.x >= p.x) ) game.conf
    in
       if (List.length right) >= 3 then
           List.concat 
               [(List.map (\q -> {q | matching =True }) right)
               ,others]
       else
           game.conf

vMatchAt: Piece -> Game -> List Piece
vMatchAt p game =
    let
        below = List.filter (\q -> q.color == p.color &&
                           q.x == p.x &&
                           q.y - p.y <= 2 &&
                           q.y >= p.y ) game.conf
        others = List.filter (\q -> not(q.color == p.color &&
                           q.x == p.x &&
                           q.y - p.y <= 2 &&
                           q.y >= p.y) ) game.conf
    in
        if (List.length below) >= 3 then
            List.concat 
               [(List.map (\q -> {q | matching =True }) below)
               ,others]
        else
            game.conf
        
match: Game -> Game
match game =
    let
        newconf = List.foldl (\p conf -> vMatchAt p {game | conf = conf}) game.conf game.conf
        newnewconf = List.foldl (\p conf -> hMatchAt p {game | conf = conf}) newconf newconf
        isMatched = List.any (\p -> p.matching) newnewconf
    in
        if isMatched then
            {game | conf = newnewconf ,state=Matching}
        else
            {game| state = Waiting}

countMatch: Game -> Int
countMatch game =
    let
        newconf = List.foldl (\p conf -> vMatchAt p {game | conf = conf}) game.conf game.conf
        newnewconf = List.foldl (\p conf -> hMatchAt p {game | conf = conf}) newconf newconf
    in
        List.length <| List.filter (\p -> p.matching) newnewconf
                

deleteMatch: Game -> Game
deleteMatch game =
    {game | conf = List.filter(\p -> not p.matching) game.conf
    ,state=Deleting
    ,elapsed=0 }

dropColumn: List Piece -> List Piece

dropColumn conf =
    List.sortBy .y conf |> List.indexedMap (\i p -> {p| y=i})
        
drop: Game -> Game

drop game =
    let
        conf = List.concat <| List.map(\j ->drop1 j game) (List.range 0 size)
    in
        {game | conf=conf
        ,state = Adding
        ,elapsed = 0}


drop1: Int -> Game -> List Piece

drop1  j game =
    
       dropColumn (List.filter (\p -> p.x==j) game.conf)

add: List Int -> Game -> Game

add rList game =
    let
        conf=game.conf
        newconf = List.concat <|
                  List.map
                  (\j -> add1 j (List.take size <| List.drop (size*j) rList) game)
                  (List.range 0 (size-1))
    in
        {game | conf = newconf
        --,state = Matching}
        ,state = Waiting}

add1: Int -> List Int -> Game -> List Piece

add1 j rList game =
    let
        column = List.filter (\p -> p.x==j) game.conf
        colors = List.take (size-(List.length column)) rList
        addend = List.indexedMap
                 (\i c -> {x = j,y=((List.length column) +i),color = c, matching = False})
                 colors

    in
        List.concat [column,addend]

            

swap: Piece -> Game -> Game
      
swap end game =
    case game.state of
        Moving start ->
            let
                others = List.filter(\p -> ((p.x/=end.x) || (p.y/=end.y)) && ((p.x/=start.x) || (p.y/=start.y))) game.conf
                distance = (abs (start.x - end.x))+(abs (start.y - end.y))
                       
            in
                if distance /= 1 then
                    {game | state = Waiting}
                else
                    {game | conf= List.concat[
                             [{start|color =end.color},{end|color=start.color}]
                            ,others]
                    ,state = game.state
                    ,elapsed = 0}
        _ -> game

numMatch: Model -> Int
numMatch model =
    case model of
        Setup -> 0
        Simulating game ->
            List.length <| List.filter .matching <| (match game).conf
        Summary game ->
            List.length <| List.filter .matching <| (match game).conf
        Playing game ->
            List.length <| List.filter .matching <| (match game).conf

swapPieces: Piece -> Piece -> List Piece -> List Piece
swapPieces p1 p2 list =
    let
        others = List.filter (\p -> not((p.x == p1.x && p.y == p1.y) ||
                                  (p.x == p2.x && p.y == p2.y))) list
    in
        List.concat [[{p1|x=p2.x, y=p2.y}]
                    ,[{p2|x=p1.x, y=p1.y}]
                    ,others]
resultsToString: List {id: Int, x:Int, y:Int, dir:Int, size:Int} -> String
resultsToString list =
    List.foldr (\r str -> str ++ (String.fromInt r.id) ++ ","
                    ++ (String.fromInt r.x) ++ ","
                    ++ (String.fromInt r.y) ++ ","
                    ++ (String.fromInt r.dir) ++ ","
                    ++(String.fromInt r.size) ++ "\n" )
        "id,x,y,dir,size\n" list
    
update msg model =
    case model of
        Summary game->
            (Summary game, Cmd.none)
        Setup ->
            let
               
                newGame = {conf =
                               (List.concat
                                    (List.map row (List.range 0 (size-1))))
                          , state = Waiting
                          , elapsed = 10
                          , simuNum = 0
                          , simuResults = []
                          }
            in
                case msg of
                    SimulationStarted -> (Simulating newGame
                                         , Random.generate Generated (Random.list (size*size) (Random.int 0 (numColor-1))))
                    Started -> (Playing newGame
                               , Random.generate Generated (Random.list (size*size) (Random.int 0 (numColor-1))))
                    _ -> (model, Cmd.none)
        Simulating game ->
            case msg of
                Generated rlist -> (Simulating (match<|  {game|conf=randomize game.conf rlist}), Cmd.none)
                Added rlist ->
                    let
                        newGame = add rlist game
                        num = countMatch newGame
                        newnewGame = {newGame | simuResults = if num > 0 then
                                                                  {x=0,y=0,dir=0,id=newGame.simuNum,size=num}::newGame.simuResults
                                                              else
                                                                  newGame.simuResults
                               }
                    in
                        (Simulating (match newnewGame), Cmd.none)
                RandSwap r->
                    let
                        idx = r // 2
                        dir = modBy 2 r
                        p1 = Maybe.withDefault {x=0,y=0,color=0,matching=False} <|
                             List.head <|
                             List.filter (\p-> p.x == modBy (size) idx && p.y == idx//(size)) game.conf
                        p2 = if dir==0 then
                                 Maybe.withDefault {x=0,y=0,color=0,matching=False} <|
                                     List.head <|
                                         List.filter (\p-> p.x == ((modBy (size) idx)+1) && p.y == idx//(size)) game.conf
                             else
                                 Maybe.withDefault {x=0,y=0,color=0,matching=False} <|
                                     List.head <|
                                         List.filter (\p-> p.x == modBy (size) idx && p.y == ((idx//(size))+1)) game.conf
                        newconf = swapPieces p1 p2 game.conf
                        num = countMatch {game|conf=newconf}
                        newGame = (if num > 0 then
                                      {game | conf = newconf
                                      , simuNum = (game.simuNum + 1)
                                      , simuResults = {x=p1.x,y=p1.y,dir=dir,id=game.simuNum,size=num}::game.simuResults}
                                  else
                                      game)
                    in
                        (Simulating (match newGame), Cmd.none)
                Elapsed t ->
                    case game.state of
                        Waiting ->
                            (Simulating game, Random.generate RandSwap (Random.int 0 (2*(size)*(size)-1)))
                        Matching ->
                            (Simulating (deleteMatch game),Cmd.none)
                        Deleting ->
                            (Simulating (drop game), Cmd.none)
                        Adding ->
                             let
                                 dummy = Debug.log "adding" t
                             in
                                 (model, Random.generate Added (Random.list (size*size) (Random.int 0 (numColor-1))))
                        other ->
                             let
                                 dummy = Debug.log "etc" game.state
                             in
                                 (model, Cmd.none)
                Quited -> (Summary game, downLoad <| resultsToString game.simuResults)
                _ -> (model, Cmd.none)
        Playing game ->
             case msg of
                 Generated rlist ->( Playing (match<|  {game|conf=randomize game.conf rlist}), Cmd.none)
                 MoveStart pdata ->(Playing {game | state = (Moving pdata)},Cmd.none)
                 MoveEnd pdata ->
                     let
                         n = numMatch (Playing (match <|swap pdata game))
                         dummy2 = if n > 0 then
                                     Debug.log "swapped" n
                                 else
                                     0
                     in
                         (Playing (match <|swap pdata game) , Cmd.none)
                 Added rlist -> (Playing (add rlist game), Cmd.none)
                 --Added rlist -> (Playing (game), Cmd.none)
                 Elapsed t ->
                     case game.state of
                         Waiting ->
                             let
                                 num = numMatch model
                                 dummy = if num > 0 then
                                             Debug.log "" num
                                         else
                                             0
                             in
                                 (Playing (match game), Cmd.none)
                         Matching ->
                             if game.elapsed > 1 then
                                 (Playing (deleteMatch game),Cmd.none)
                             else
                                 (Playing {game | elapsed = (game.elapsed+1)},Cmd.none)
                         Deleting ->
                             if game.elapsed > 1 then
                                 (Playing (drop game)
                                 ,Cmd.none)
                             else
                                 (Playing {game | elapsed = (game.elapsed+1)},Cmd.none)
                         Adding ->
                             if game.elapsed > 1 then
                                 (model
                                 ,Random.generate Added (Random.list (size*size) (Random.int 0 (numColor-1))))
                             else
                                 (Playing {game | elapsed = (game.elapsed+1)},Cmd.none)
                         _ -> (Playing {game | elapsed = (game.elapsed+1)},Cmd.none)
                 _ ->(model,Cmd.none)


randomize conf rlist =
    List.indexedMap (\idx p -> {p | color = Maybe.withDefault 0
                                    (List.head (List.drop idx rlist))}) conf
    
pallet colornum =
    case colornum of
        0 -> "violet"
        1 -> "pink"
        2 -> "skyblue"
        3 -> "lightgreen"
        4 -> "orange"
        _ -> "black"
             
piece: Piece -> Svg Msg
       
piece pdata =
    let
        unit = 20
    in
        circle [cx (String.fromInt (pdata.x*unit+2*unit))
               ,cy (String.fromInt ((size*unit)-pdata.y*unit+2*unit))
               ,r (String.fromInt (unit//2))
               ,fill ( pallet pdata.color)
               ,stroke (if pdata.matching then
                            "yellow"
                        else
                            "white")
               ,strokeWidth (if pdata.matching then
                                 "3"
                             else
                                 "0")
               ,onMouseDown (MoveStart pdata)
               ,onMouseUp (MoveEnd pdata)]
        []
        
view model =
        case model of
            Summary game ->
                Html.div[]
                    [
                     Html.button [][text "Download"]
                    ]
            Setup ->
                Html.div [][
                     Html.button [onClick Started][text "Start"]
                    ,Html.button [onClick SimulationStarted][text "Start Simulation"]
                    ]
            Simulating game ->
                Html.div [][
                     Html.button [onClick (Quited )][text "Quit"]
                    ,Html.br[][]
                    ,gameSvg game]
            Playing game ->
                gameSvg game

gameSvg: Game -> Html.Html Msg
gameSvg game =
    svg [width "800"
        ,height "800"
        ]
    (List.concat[[]
         ,(List.map piece game.conf)
         ]
    )
    
subscriptions: Model -> Sub Msg
subscriptions model =
    Time.every 1 Elapsed

