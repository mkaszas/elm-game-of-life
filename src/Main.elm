module Main exposing (Model, Msg(..), init, main, update, viewDocument)

import Browser
import Html exposing (Html)
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


type alias Model =
    { grid : Grid
    }


type Msg
    = Generate Time.Posix


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Game of life"
    , body = [ view model ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 100 Generate


init : x -> ( Model, Cmd Msg )
init _ =
    ( { grid = initialGrid }, Cmd.none )


initialGrid : Grid
initialGrid =
    [ ( 30, 30 ), ( 31, 30 ), ( 32, 30 ), ( 30, 29 ), ( 31, 32 ) ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { grid } =
    case msg of
        Generate _ ->
            ( { grid = nextGeneration grid }, Cmd.none )


view : Model -> Html Msg
view { grid } =
    svg
        [ width "600"
        , height "600"
        ]
        (List.map showPoint grid)


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
