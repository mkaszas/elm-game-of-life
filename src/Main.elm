module Main exposing (Model, Msg(..), init, main, update, viewDocument)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as EList
import Svg exposing (Attribute, Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = viewDocument
        , update = update
        , subscriptions = subscriptions
        }


type alias Point =
    ( Int, Int )


type alias Grid =
    List Point


type State
    = Paused
    | Running


type alias Model =
    { grid : Grid
    , state : State
    , speed : Float
    }


type Msg
    = Generate
    | Start
    | Stop
    | Reset
    | SetSpeed String


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Game of life"
    , body = [ view model ]
    }


subscriptions : Model -> Sub Msg
subscriptions { state, speed } =
    case state of
        Paused ->
            Sub.none

        Running ->
            Time.every speed <| always Generate


init : x -> ( Model, Cmd Msg )
init _ =
    ( { grid = initialGrid, state = Paused, speed = 100 }, Cmd.none )


initialGrid : Grid
initialGrid =
    [ ( 30, 30 ), ( 31, 30 ), ( 32, 30 ), ( 30, 29 ), ( 31, 32 ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        Generate ->
            ( { model | grid = nextGeneration model.grid }, Cmd.none )

        Start ->
            ( { model | state = Running }, Cmd.none )

        Stop ->
            ( { model | state = Paused }, Cmd.none )

        SetSpeed s ->
            ( { model | speed = parseSpeed s }, Cmd.none )


view : Model -> Html.Html Msg
view { grid, speed } =
    Html.div []
        [ svg
            [ width "600"
            , height "600"
            ]
            (List.map showPoint grid)
        , Html.button [ onClick Start ] [ text "Start" ]
        , Html.button [ onClick Stop ] [ text "Stop" ]
        , Html.button [ onClick Generate ] [ text "Step" ]
        , Html.button [ onClick Reset ] [ text "Reset" ]
        , Html.input
            [ type_ "range"
            , Attr.min "600"
            , Attr.max "1000"
            , Attr.step "50"
            , Attr.value (showSpeed speed)
            , onInput SetSpeed
            ]
            []
        ]


showSpeed : Float -> String
showSpeed s =
    String.fromInt (1000 - round s)


parseSpeed : String -> Float
parseSpeed s =
    String.toFloat s
        |> Maybe.withDefault 900
        |> (\x -> 1000 - x)


getSvgAttributes : Point -> List (Attribute msg)
getSvgAttributes ( xCoord, yCoord ) =
    let
        defaultAttributes =
            [ width "10", height "10", fill "black" ]

        multiplier =
            12

        transformCoord c =
            String.fromInt (c * multiplier)
    in
    [ x <| transformCoord xCoord, y <| transformCoord yCoord ] ++ defaultAttributes


showPoint : Point -> Svg msg
showPoint p =
    rect (getSvgAttributes p) []


nextGeneration : Grid -> Grid
nextGeneration grid =
    let
        -- Only check cells that are alive & their neighbours.
        gridToCheck =
            grid
                |> List.concatMap (\p -> p :: getNeighbours p)
                |> EList.unique
    in
    List.filter (isAliveInNextGeneration grid) gridToCheck


getNeighbours : Point -> Grid
getNeighbours ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
        |> List.filter (\( a, b ) -> a >= 0 && a < 50 && b >= 0 && b < 50)


isAliveInNextGeneration : Grid -> Point -> Bool
isAliveInNextGeneration grid ( x, y ) =
    let
        isAlive p =
            List.member p grid

        isThisAlive =
            isAlive ( x, y )

        neighbours =
            getNeighbours ( x, y )

        numberOfAliveNeighbours =
            neighbours
                |> List.filter isAlive
                |> List.length
    in
    checkRules isThisAlive numberOfAliveNeighbours


checkRules : Bool -> Int -> Bool
checkRules isAlive n =
    if isAlive then
        n == 2 || n == 3

    else
        n == 3
