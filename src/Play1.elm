module Play1 exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Color exposing (Color)
import Grid exposing (fold2d)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random exposing (..)
import Random.Extra exposing (result)
import Simplex exposing (PermutationTable)
import Time exposing (Posix)


type alias Model =
    { seed : Random.Seed
    , count : Float
    }


type Msg
    = AnimationFrame Posix



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42



-- Create a function for 2D fractal noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 4.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable


main : Program Float Model Msg
main =
    Browser.element
        { init = \floatSeed -> ( { seed = Random.initialSeed (floor (floatSeed * 10000)), count = 0 }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame AnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            ( { model | count = model.count + 1 }, Cmd.none )


h : number
h =
    1200


w : number
w =
    h


cellSize : Float
cellSize =
    h / numRows


numRows : number
numRows =
    50


numCols : number
numCols =
    numRows


view : Model -> Html Msg
view model =
    let
        artWork =
            drawPiece [] model
    in
    Canvas.toHtml
        ( w, h )
        []
        artWork


drawPiece : List Renderable -> Model -> List Renderable
drawPiece items model =
    Grid.fold2d
        { rows = numRows, cols = numCols }
        renderItem
        items


renderItem : ( Int, Int ) -> List Renderable -> List Renderable
renderItem ( col, row ) items =
    let
        ( colf, rowf ) =
            ( toFloat col, toFloat row )

        ( x, y ) =
            ( rowf * cellSize
            , colf * cellSize
            )

        red =
            noise x y

        blue =
            noise y x
    in
    shapes
        [ fill (Color.rgba red 0 blue 1) ]
        [ circle ( x, y ) (cellSize / 1.75) ]
        :: items
