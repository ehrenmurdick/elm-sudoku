module Main exposing (..)

import Html exposing (Html, button, div, input, program, text)
import Html.Attributes exposing (attribute, id, style, value)
import Html.Events exposing (onClick, onInput)
import List exposing (all, any, concatMap, drop, filter, head, length, map, map2, range, repeat, reverse, take)
import Maybe
import Result exposing (withDefault)
import Set exposing (diff, fromList, isEmpty, size)
import String exposing (toInt)
import Time
import Tuple exposing (first, second)


width : Int
width =
    4


sqrtWidth : Int
sqrtWidth =
    2

totalEntryCount : Int
totalEntryCount =
    width * width

sqIndx : Int
sqIndx =
    1

type Msg
    = Update Int Int
    | Solve
    | Tick Time.Time


type alias Move =
    { board : Board, idx : Int }


type alias Model =
    { current : Board
    , nextMoves : List Move
    , valid : Bool
    , solving : Bool
    , lastChangeIdx : Int
    }


type alias Board =
    List Entry


emptyBoard : Board
emptyBoard =
    map2 (,)
        (range 0 (totalEntryCount - 1))
        (repeat totalEntryCount 0)


init : ( Model, Cmd Msg )
init =
    ( { current = emptyBoard
      , nextMoves = nextMoves emptyBoard
      , valid = True
      , solving = False
      , lastChangeIdx = -1
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.solving of
        True ->
            Time.every (Time.millisecond * 1) Tick

        False ->
            Sub.none



-- let
--     f ( idx, _ ) =
--         ( idx, ((idx // width) + (idx % width)) % 9 + 1 )
-- in
--     map f emptyModel


type alias Entry =
    ( Int, Int )


getFirstEmpty : Board -> Maybe Int
getFirstEmpty model =
    case model of
        [] ->
            Nothing

        ( idx, val ) :: ms ->
            if val == 0 then
                Just idx
            else
                getFirstEmpty ms


allPossibleMoves : Int -> Board -> List Move
allPossibleMoves idx model =
    map (doMove idx model) (range 1 width)


validMove : Int -> Move -> Bool
validMove idx move =
    let
        board =
            move.board

        ss =
            squareAt ( idx, 0 ) board

        cs =
            colAt ( idx, 0 ) board

        rs =
            rowAt ( idx, 0 ) board

        allSets =
            [ ss, cs, rs ]
    in
        all setIsValid allSets


nextMoves : Board -> List Move
nextMoves model =
    case getFirstEmpty model of
        Nothing ->
            []

        Just idx ->
            filter (validMove idx) (allPossibleMoves idx model)


complete : Board -> Bool
complete model =
    let
        nonzero =
            \( idx, val ) -> val /= 0
    in
        all nonzero model


findDone : List Board -> Maybe Board
findDone modelList =
    head (filter complete modelList)


parseInput : Int -> String -> Msg
parseInput idx string =
    let
        value =
            toInt string
                |> withDefault 0

        f x =
            if x >= 10 then
                f (x - 10)
            else
                x
    in
        Update idx (f value)


entryStyle : Entry -> Model -> List ( String, String )
entryStyle entry model =
    let
        rowIsValid =
            setIsValid (rowAt entry model.current)

        borderTop =
            if rowIsValid then
                "1px solid white"
            else
                "1px solid red"

        colIsValid =
            setIsValid (colAt entry model.current)

        borderLeft =
            if colIsValid then
                "1px solid white"
            else
                "1px solid red"

        sqIsValid =
            setIsValid (squareAt entry model.current)

        background =
            if sqIsValid then
                "white"
            else
                "pink"

        isLastChange =
            (first entry) == model.lastChangeIdx

        shadow =
            if isLastChange then
                "0 0 3px blue"
            else
                ""

        transition =
            if isLastChange then
                ""
            else
                "box-shadow 1s"
    in
        [ ( "border-top", borderTop )
        , ( "border-left", borderLeft )
        , ( "border-right", borderLeft )
        , ( "border-bottom", borderTop )
        , ( "background-color", background )
        , ( "box-shadow", shadow )
        , ( "transition", transition )
        ]


showEntry : Model -> Entry -> Html Msg
showEntry model ( idx, int ) =
    input
        [ id ("item-" ++ toString idx)
        , attribute "data-x" (toString (idx // width))
        , attribute "data-y" (toString (idx % width))
        , attribute "data-sq" (toString (squareOf ( idx, int )))
        , value (toString int)
        , style
            ([ ( "width", "1em" )
             , ( "margin", "5px" )
             ]
                ++ (entryStyle ( idx, int ) model)
            )
        , onInput (parseInput idx)
        ]
        []


setsOfGroup : (Entry -> Int) -> Int -> Board -> List Board
setsOfGroup lookup setsLength model =
    map
        (\i ->
            filter
                (\entry ->
                    lookup entry == i
                )
                model
        )
        (range 0 setsLength)


rows : Board -> List Board
rows =
    setsOfGroup rowOf (width - 1)


cols : Board -> List Board
cols =
    setsOfGroup colOf (width - 1)


squares : Board -> List Board
squares =
    setsOfGroup squareOf sqIndx


containingSet : (Entry -> Int) -> Entry -> List Entry -> List Entry
containingSet f entry model =
    let
        sameSet a =
            f a == f entry
    in
        filter sameSet model


rowAt : Entry -> List Entry -> List Entry
rowAt =
    containingSet rowOf


colAt : Entry -> List Entry -> List Entry
colAt =
    containingSet colOf


squareAt : Entry -> List Entry -> List Entry
squareAt =
    containingSet squareOf


showRow : Model -> List Entry -> Html Msg
showRow model row =
    div [] (map (showEntry model) row)


squareOf : Entry -> Int
squareOf ( idx, _ ) =
    (((idx % width) // sqrtWidth) * sqrtWidth) + (idx // width) // sqrtWidth


rowOf : Entry -> Int
rowOf ( idx, _ ) =
    idx % width


colOf : Entry -> Int
colOf ( idx, _ ) =
    idx // width


setIsValid : List Entry -> Bool
setIsValid es =
    let
        nonzero ( idx, val ) =
            val /= 0

        entryList =
            map second (filter nonzero es)

        entrySet =
            fromList entryList
    in
        (size entrySet) == (length entryList)


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style
                [ ( "padding", "3em" )
                , ( "display", "inline-block" )
                ]
            ]
            (map (showRow model) (rows model.current))
        , button [ onClick Solve ] [ text "solve" ]
        ]


updateIndex : Int -> Int -> Entry -> Entry
updateIndex idx value ( i, v ) =
    if i == idx then
        ( i, value )
    else
        ( i, v )


doMove : Int -> Board -> Int -> Move
doMove idx board newValue =
    { board = map (updateIndex idx newValue) board
    , idx = idx
    }


stepMoves : List Move -> List Move
stepMoves boardList =
    case boardList of
        [] ->
            []

        m :: ms ->
            (nextMoves m.board) ++ ms


done : Model -> Bool
done model =
    not (any (\( _, v ) -> v == 0) model.current)



-- done : Model -> Bool
-- done model =
--     let
--         nonzero ( idx, v ) =
--             v /= 0
--     in
--         all nonzero model.current
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Update idx value ->
            ( { model
                | current = (doMove idx model.current value).board
                , lastChangeIdx = idx
              }
            , Cmd.none
            )

        Tick _ ->
            if done model then
                ( { model | solving = False }, Cmd.none )
            else
                case model.nextMoves of
                    [] ->
                        ( model, Cmd.none )

                    m :: ms ->
                        ( { model
                            | current = m.board
                            , nextMoves = stepMoves model.nextMoves
                            , lastChangeIdx = m.idx
                          }
                        , Cmd.none
                        )

        Solve ->
            ( { model
                | solving = not model.solving
                , nextMoves = nextMoves model.current
              }
            , Cmd.none
            )


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
