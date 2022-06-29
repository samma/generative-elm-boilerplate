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
import Random
import Random.Extra exposing (result)
import Simplex exposing (PermutationTable)
import Task
import Time exposing (Posix)


type alias Model =
    { seed : Random.Seed
    , heading : Float -- heading in radians
    , speed : Float -- speed in pixels per second
    }



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42



-- Create a function for 2D fractal noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 4.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable


main : Program Float Model ()
main =
    Browser.element
        { init = \floatSeed -> ( { seed = Random.initialSeed (floor (floatSeed * 10000)), heading = 0.0, speed = 0.0 }, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        }


h : number
h =
    1200


w : number
w =
    h


cellSize =
    h / numRows


numRows =
    200


numCols =
    numRows


view : Model -> Html ()
view model =
    let
        artWork =
            drawPiece []
    in
    Canvas.toHtml
        ( w, h )
        []
        artWork


drawPiece shapes =
    Grid.fold2d
        { rows = numRows, cols = numCols }
        renderItem
        []


renderItem ( col, row ) lines =
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
        [ rect ( x, y ) cellSize cellSize ]
        :: lines



{-
   [(0,0),(1,0),(2,0),(0,1),(1,1),(2,1)]
-}
--permTable : PermutationTable
--permTable =
--    Simplex.permutationTableFromInt 42
--noise : Float -> Float -> Float
--noise =
--    Simplex.fractal2d { scale = 4.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable
