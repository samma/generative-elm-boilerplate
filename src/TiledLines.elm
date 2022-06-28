module TiledLines exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Line exposing (..)
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Simplex exposing (PermutationTable)
import Task
import Time exposing (Posix)



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
        { init = \floatSeed -> ( { seed = Random.initialSeed (floor (floatSeed * 10000)) }, Cmd.none )
        , update = \_ m -> ( m, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = view
        }


h : number
h =
    1200


w : number
w =
    1200


padding : number
padding =
    30


type alias Point =
    ( Float, Float )


type alias Line =
    ( Point, Point )


type alias Model =
    { seed : Random.Seed
    }


view : Model -> Html ()
view model =
    Canvas.toHtml
        ( w, h )
        [-- center it
        ]
        [ shapes [ fill Color.black ] [ rect ( 0, 0 ) w h ]
        , shapes
            [ stroke Color.brown
            , lineWidth 8
            , lineCap RoundCap
            ]
            (drawLines model.seed 0 [])
        , shapes
            [ stroke Color.darkRed
            , lineWidth 4
            , lineCap RoundCap
            , lineDash [ 30 ]
            ]
            (drawLines (Random.initialSeed (floor (1 * 10000))) 0 [])
        ]


step =
    30


cols =
    w // step


rows =
    h // step


drawLines : Random.Seed -> Int -> List Shape -> List Shape
drawLines seed i shapes =
    if i > cols * rows then
        shapes

    else
        let
            x =
                modBy cols i

            y =
                i // rows

            ( line, seed2 ) =
                randomLine seed (toFloat x * step) (toFloat y * step) step step

            lineShapes =
                drawLine line seed
        in
        drawLines seed2 (i + 1) (lineShapes :: shapes)


randomLine seed x y width height =
    let
        ( a, seed2 ) =
            Random.step randomBool seed
    in
    if a then
        horizontalLine seed2 x y width height

    else
        diagonalLine seed2 x y width height


randomBool =
    Random.map (\n -> n < 0.97) (Random.float 0 1)


diagonalLine seed x y width height =
    Random.step randomBool seed
        |> Tuple.mapFirst
            (\bool ->
                if bool then
                    ( ( x, y ), ( x + width, y + height ) )

                else
                    ( ( x + width, y ), ( x, y + height ) )
            )


horizontalLine seed x y width height =
    Random.step randomBool seed
        |> Tuple.mapFirst
            (\bool ->
                if bool then
                    ( ( x + width / 2, y ), ( x + width / 2, y + height ) )

                else
                    ( ( x, y + height / 2 ), ( x + width, y + height / 2 ) )
            )


drawLine ( start, end ) seed =
    let
        ( a, seed2 ) =
            Random.step randomBool seed
    in
    if a then
        path start [ lineTo end ]

    else
        circle start (step * 1.5)



--circle start 20
--rect start 10 10


noiseModify ( x, y ) =
    noise x y
