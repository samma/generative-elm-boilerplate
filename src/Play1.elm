module Play1 exposing (main)

-- Trying to implement a reaction from Scientific_Computing_Simulations_and_Modeling

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
    , count : Int
    , a : Float
    , b : Float
    , c : Float
    , d : Float
    , h : Float
    , k : Float
    , uVals : List ReactionValue
    , vVals : List ReactionValue
    }


type alias ReactionValue =
    { x : Int
    , y : Int
    , value : Float
    }


type Msg
    = AnimationFrame Posix


main : Program Float Model Msg
main =
    Browser.element
        { init = \floatSeed -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


rVal : Int -> Int -> Float -> ReactionValue
rVal =
    \x y v ->
        { x = x
        , y = y
        , value = v
        }


initEmptyVals : Int -> List ReactionValue
initEmptyVals n =
    let
        zero =
            toFloat 0
    in
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> rVal x y zero :: result)
        []


init : ( Model, Cmd Msg )
init =
    let
        e =
            initEmptyVals gridSize

        f =
            initEmptyVals gridSize
    in
    ( { seed = Random.initialSeed (floor (42 * 10000))
      , count = 0
      , a = 1
      , b = -1
      , c = 2
      , d = -1.5
      , h = 1
      , k = 1
      , uVals = e
      , vVals = f
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame AnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            if model.count < 50 then
                ( { model | count = model.count + 1 }, Cmd.none )

            else
                ( model, Cmd.none )


h : number
h =
    1200


w : number
w =
    h


gridSize : number
gridSize =
    100


cellSize : Float
cellSize =
    h / gridSize


delta_h : Float
delta_h =
    1.0 / gridSize


delta_t : Float
delta_t =
    0.02


view : Model -> Html Msg
view model =
    let
        artWork =
            drawPiece [] model
    in
    Canvas.toHtml
        ( w, h )
        []
        (shapes
            [ fill Color.black ]
            [ rect ( 0, 0 ) w h ]
            :: artWork
        )


drawPiece : List Renderable -> Model -> List Renderable
drawPiece items model =
    Grid.fold2d
        { rows = gridSize, cols = gridSize }
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



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42



-- Create a function for 2D fractal noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 4.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable
