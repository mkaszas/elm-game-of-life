module Main exposing (Model, Msg(..), init, main, update, viewDocument)

import Browser
import Dict as Dict exposing (..)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onInput)
import List.Extra as EList
import Svg exposing (Attribute, Svg, circle, g, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, width, x, y)
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


type alias InactiveGrid =
    Dict Point Bool


type State
    = Paused InactiveGrid
    | Running Grid


type alias Model =
    { state : State
    , speed : Float
    }


type Msg
    = Generate
    | Start
    | Stop
    | Reset
    | SetSpeed String
    | TogglePoint Point


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Game of life"
    , body = [ view model ]
    }


subscriptions : Model -> Sub Msg
subscriptions { state, speed } =
    case state of
        Paused _ ->
            Sub.none

        Running _ ->
            Time.every speed <| always Generate


init : x -> ( Model, Cmd Msg )
init _ =
    ( { state = Paused initialGrid, speed = 100 }, Cmd.none )


emptyGrid : InactiveGrid
emptyGrid =
    List.range 0 49
        |> List.concatMap (\x -> List.map (\y -> ( ( x, y ), False )) <| List.range 0 49)
        |> Dict.fromList


initialGrid : InactiveGrid
initialGrid =
    toInactiveGrid [ ( 30, 30 ), ( 31, 30 ), ( 32, 30 ), ( 30, 29 ), ( 31, 32 ) ]


toInactiveGrid : Grid -> InactiveGrid
toInactiveGrid g =
    emptyGrid
        |> Dict.map (\p _ -> List.member p g)


toActiveGrid : InactiveGrid -> Grid
toActiveGrid =
    Dict.keys << Dict.filter (always identity)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reset ->
            init ()

        Generate ->
            case model.state of
                Paused ig ->
                    ( { model | state = Paused <| toInactiveGrid << nextGeneration << toActiveGrid <| ig }, Cmd.none )

                Running g ->
                    ( { model | state = Running <| nextGeneration g }, Cmd.none )

        Start ->
            case model.state of
                Paused ig ->
                    ( { model | state = Running <| toActiveGrid ig }, Cmd.none )

                Running _ ->
                    ( model, Cmd.none )

        Stop ->
            case model.state of
                Paused _ ->
                    ( model, Cmd.none )

                Running g ->
                    ( { model | state = Paused <| toInactiveGrid g }, Cmd.none )

        SetSpeed s ->
            ( { model | speed = parseSpeed s }, Cmd.none )

        TogglePoint p ->
            case model.state of
                Paused grid ->
                    ( { model | state = Paused <| Dict.update p (Maybe.map not) grid }, Cmd.none )

                Running _ ->
                    ( model, Cmd.none )


view : Model -> Html.Html Msg
view { state, speed } =
    Html.div []
        [ svg
            [ width "600"
            , height "600"
            ]
            (showGrid state)
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


showGrid : State -> List (Html.Html Msg)
showGrid state =
    case state of
        Paused ig ->
            Dict.values
                (Dict.map
                    (\p a ->
                        if a then
                            showPoint p

                        else
                            showEmptySpace p
                    )
                    ig
                )

        Running g ->
            List.map showPoint g


showSpeed : Float -> String
showSpeed s =
    String.fromInt (1000 - round s)


parseSpeed : String -> Float
parseSpeed s =
    String.toFloat s
        |> Maybe.withDefault 900
        |> (\x -> 1000 - x)


getSvgCoordinates : Point -> List (Attribute Msg)
getSvgCoordinates ( xCoord, yCoord ) =
    let
        multiplier =
            12

        transformCoord c =
            String.fromInt (c * multiplier)
    in
    [ x <| transformCoord xCoord, y <| transformCoord yCoord ]


getSvgAttributes : Point -> List (Attribute Msg)
getSvgAttributes p =
    getSvgCoordinates p ++ [ width "10", height "10", fill "black", onClick (TogglePoint p) ]


showPoint : Point -> Svg Msg
showPoint p =
    rect (getSvgAttributes p) []


showEmptySpace : Point -> Svg Msg
showEmptySpace p =
    let
        circleAttrs ( x, y ) =
            [ cx (String.fromInt (x * 12 + 5)), cy (String.fromInt (y * 12 + 5)), r "1", fill "red", onClick (TogglePoint p) ]
    in
    g []
        [ rect (getSvgCoordinates p ++ [ width "10", height "10", fill "white", onClick (TogglePoint p) ]) []
        , circle (circleAttrs p) []
        ]


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
