module Main exposing (Model, Msg(..), init, main, update, viewDocument)

import Browser
import Html as Html
import Html.Events exposing (onClick)
import List.Extra as EList
import Svg exposing (..)
import Svg.Attributes exposing (..)
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
    }


type Msg
    = Generate
    | Start
    | Stop


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Game of life"
    , body = [ view model ]
    }


subscriptions : Model -> Sub Msg
subscriptions { state } =
    case state of
        Paused ->
            Sub.none

        Running ->
            Time.every 100 <| always Generate


init : x -> ( Model, Cmd Msg )
init _ =
    ( { grid = initialGrid, state = Paused }, Cmd.none )


initialGrid : Grid
initialGrid =
    [ ( 30, 30 ), ( 31, 30 ), ( 32, 30 ), ( 30, 29 ), ( 31, 32 ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( { model | grid = nextGeneration model.grid }, Cmd.none )

        Start ->
            ( { model | state = Running }, Cmd.none )

        Stop ->
            ( { model | state = Paused }, Cmd.none )


view : Model -> Html.Html Msg
view { grid } =
    Html.div []
        [ svg
            [ width "600"
            , height "600"
            ]
            (List.map showPoint grid)
        , Html.button [ onClick Start ] [ text "Start" ]
        , Html.button [ onClick Stop ] [ text "Stop" ]
        , Html.button [ onClick Generate ] [ text "Step" ]
        ]


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
