module Main exposing (..)

import String exposing (toInt)
import Result exposing (withDefault)
import Tuple exposing (second)
import Maybe
import Set
    exposing
        ( fromList
        , diff
        , isEmpty
        , size
        )
import List
    exposing
        ( all
        , drop
        , filter
        , length
        , map
        , map2
        , range
        , repeat
        , take
        )
import Html
    exposing
        ( beginnerProgram
        , div
        , Html
        , input
        , text
        )
import Html.Attributes
    exposing
        ( id
        , attribute
        , style
        , value
        )
import Html.Events exposing (onInput)


width : Int
width =
    9


totalEntryCount : Int
totalEntryCount =
    width * width


type Msg
    = Update Int Int


type alias Model =
    List Entry


emptyModel : Model
emptyModel =
    map2 (,)
        (range 0 (totalEntryCount - 1))
        (repeat totalEntryCount 0)


model : Model
model =
    emptyModel



-- let
--     f ( idx, _ ) =
--         ( idx, ((idx // width) + (idx % width)) % 9 + 1 )
-- in
--     map f emptyModel


type alias Entry =
    ( Int, Int )


getFirstEmpty : Model -> Maybe Int
getFirstEmpty model =
    case model of
        [] ->
            Nothing

        ( idx, val ) :: ms ->
            if val == 0 then
                Just idx
            else
                getFirstEmpty ms


doMove : Int -> Model -> Int -> Model
doMove idx model newValue =
    let
        move =
            Update idx newValue
    in
        update move model


allPossibleMoves : Int -> Model -> List Model
allPossibleMoves idx model =
    map (doMove idx model) (range 1 9)


validMove : Model -> Bool
validMove model =
    let
        ss =
            squares model

        cs =
            cols model

        rs =
            rows model

        allSets =
            ss ++ cs ++ rs
    in
        all setIsValid allSets


nextMoves : Model -> List Model
nextMoves model =
    case getFirstEmpty model of
        Nothing ->
            []

        Just idx ->
            filter validMove (allPossibleMoves idx model)


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
            setIsValid (rowAt entry model)

        borderTop =
            if rowIsValid then
                "1px solid white"
            else
                "1px solid red"

        colIsValid =
            setIsValid (colAt entry model)

        borderLeft =
            if colIsValid then
                "1px solid white"
            else
                "1px solid red"

        sqIsValid =
            setIsValid (squareAt entry model)

        background =
            if sqIsValid then
                "white"
            else
                "pink"
    in
        [ ( "border-top", borderTop )
        , ( "border-left", borderLeft )
        , ( "border-right", borderLeft )
        , ( "border-bottom", borderTop )
        , ( "background-color", background )
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


setsOfGroup : (Entry -> Int) -> Int -> Model -> List Model
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


rows : Model -> List Model
rows =
    setsOfGroup rowOf 8


cols : Model -> List Model
cols =
    setsOfGroup colOf 8


squares : Model -> List Model
squares =
    setsOfGroup squareOf 2


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
    (((idx % 9) // 3) * 3) + (idx // 9) // 3


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
    div
        [ style
            [ ( "padding", "3em" )
            , ( "display", "inline-block" )
            ]
        ]
        (map (showRow model) (rows model))


updateIndex : Int -> Int -> Entry -> Entry
updateIndex idx value ( i, v ) =
    if i == idx then
        ( i, value )
    else
        ( i, v )


update : Msg -> Model -> Model
update msg model =
    case msg of
        Update idx value ->
            map (updateIndex idx value) model


main : Program Never Model Msg
main =
    beginnerProgram
        { model = model
        , view = view
        , update = update
        }
