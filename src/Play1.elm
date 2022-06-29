module Play1 exposing (main)

-- Trying to implement a reaction from Scientific_Computing_Simulations_and_Modeling

import Array exposing (..)
import Axis2d exposing (x)
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
    Grid.fold2d
        { rows = n, cols = n }
        --(\( x, y ) result -> rVal x y (10 * noise (toFloat x) (toFloat y)) :: result)
        (\( x, y ) result -> rVal x y 0 :: result)
        []


init : ( Model, Cmd Msg )
init =
    ( { seed = Random.initialSeed (floor (42 * 10000))
      , count = 0
      , a = 1
      , b = -1
      , c = 2
      , d = -1.5
      , h = 1
      , k = 1
      , uVals = initEmptyVals gridSize
      , vVals = initEmptyVals gridSize
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
            if model.count < maxIter then
                ( iterateModel model, Cmd.none )

            else
                ( model, Cmd.none )


h : number
h =
    1200


w : number
w =
    h


maxIter : Int
maxIter =
    1000


gridSize : number
gridSize =
    20


cellSize : Float
cellSize =
    h / gridSize


delta_h : Float
delta_h =
    1.0 / gridSize


delta_t : Float
delta_t =
    0.05


delta_u : Float
delta_u =
    0.0001


delta_v : Float
delta_v =
    0.0006


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( w, h )
        []
        [ shapes
            []
            (drawPiece
                []
                model
            )
        ]


iterateModel : Model -> Model
iterateModel model =
    let
        newMod =
            nextVals model
    in
    { model | uVals = Tuple.first newMod, vVals = Tuple.second newMod, count = model.count + 1 }


nextVals : Model -> ( List ReactionValue, List ReactionValue )
nextVals model =
    let
        uArr =
            fromList model.uVals

        vArr =
            fromList model.vVals

        getCenter x y arr =
            Maybe.withDefault (rVal x y 0) (get (coordToIndex ( x, y )) arr)

        getUp x y arr =
            getCenter x (y - 1) arr

        getDown x y arr =
            getCenter x (y + 1) arr

        getLeft x y arr =
            getCenter (x - 1) y arr

        getRight x y arr =
            getCenter (x + 1) y arr

        uLap x y =
            ((getRight x y uArr).value + (getLeft x y uArr).value + (getUp x y uArr).value + (getDown x y uArr).value - 4 * (getCenter x y uArr).value) / (delta_h ^ 2)

        vLap x y =
            ((getRight x y vArr).value + (getLeft x y vArr).value + (getUp x y vArr).value + (getDown x y vArr).value - 4 * (getCenter x y vArr).value) / (delta_h ^ 2)

        next_u r =
            rVal r.x r.y ((getCenter r.x r.y uArr).value + ((model.a * ((getCenter r.x r.y uArr).value - model.h)) + (model.b * (getCenter r.x r.y vArr).value - model.k) + delta_u * uLap r.x r.y) * delta_t)

        next_v r =
            rVal r.x r.y ((getCenter r.x r.y vArr).value + ((model.c * ((getCenter r.x r.y uArr).value - model.h)) + (model.d * (getCenter r.x r.y vArr).value - model.k) + delta_v * vLap r.x r.y) * delta_t)
    in
    ( List.map next_u model.uVals, List.map next_v model.vVals )


indexToCoord : Int -> ( Int, Int )
indexToCoord i =
    ( modBy i gridSize, i // gridSize )


coordToIndex : ( Int, Int ) -> Int
coordToIndex ( x, y ) =
    x + y * gridSize


drawPiece : List Renderable -> Model -> List Shape
drawPiece items model =
    List.map drawPieceItem model.vVals


drawPieceItem : ReactionValue -> Shape
drawPieceItem r =
    if r.value > 0.0 then
        circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) (10 * r.value)

    else
        circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) 0



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42



-- Create a function for 2D fractal noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 4.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable
