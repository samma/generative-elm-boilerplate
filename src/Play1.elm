module Play1 exposing (main)

-- Trying to implement a reaction from Scientific_Computing_Simulations_and_Modeling
-- TODO, merge uVals and vVals into one with x y u v
-- Adjust colors to match the new data structure, use two channels
-- Figure out good randomizations for the initial conditions

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
    , cells : List ReactionValue
    , prevCells : List ReactionValue
    }


type alias ReactionValue =
    { x : Int
    , y : Int
    , uValue : Float
    , vValue : Float
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


rVal : a -> b -> c -> d -> { x : a, y : b, uValue : c, vValue : d }
rVal =
    \x y v u ->
        { x = x
        , y = y
        , uValue = v
        , vValue = u
        }


initReactionValues : Int -> List ReactionValue
initReactionValues n =
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> rVal x y (1 + (0.03 * noise (toFloat x) (toFloat y))) (0.03 * noise (toFloat x) (toFloat y)) :: result)
        []


initZeroReactionValues : Int -> List ReactionValue
initZeroReactionValues n =
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> rVal x y 0 0 :: result)
        []


seedCorner : ( Int, Int ) -> List ReactionValue -> List ReactionValue
seedCorner =
    \( x, y ) result ->
        if x == 0 && y == 0 then
            rVal x y 1 0.7 :: result

        else
            rVal x y 0 0 :: result


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
      , cells = initReactionValues gridSize
      , prevCells = initZeroReactionValues gridSize
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
                ( iterateModel model |> iterateModel |> iterateModel |> iterateModel |> iterateModel, Cmd.none )

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
    10000


gridSize : number
gridSize =
    50


cellSize : Float
cellSize =
    h / gridSize


delta_h : Float
delta_h =
    1.0 / gridSize


delta_t : Float
delta_t =
    0.02


diff_u : Float
diff_u =
    0.0001


diff_v : Float
diff_v =
    0.0006


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( w, h )
        []
        (drawItAll
            model
        )


drawItAll : Model -> List Renderable
drawItAll model =
    List.map drawPieceItem model.cells


scaleVvals x =
    0.5 + (x / 10)


scaleUvals x =
    0.5 + (x / 10)


drawPieceItem : ReactionValue -> Renderable
drawPieceItem r =
    shapes [ fill (Color.rgba (scaleUvals r.uValue) 0 0 1) ] [ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]


clamp : Float -> Float -> Float -> Float
clamp =
    \x min max ->
        if x < min then
            min

        else if x > max then
            max

        else
            x


iterateModel : Model -> Model
iterateModel model =
    let
        nextCells =
            nextVals model
    in
    { model | prevCells = model.cells, cells = nextCells, count = model.count + 1 }


nextVals : Model -> List ReactionValue
nextVals model =
    let
        reactionArr =
            fromList model.cells

        getCenter x y arr =
            Maybe.withDefault (rVal x y 0 0) (get (coordToIndex ( x, y )) arr)

        getUp x y arr =
            if y /= 0 then
                getCenter x (y - 1) arr

            else
                getCenter x 0 arr

        getDown x y arr =
            if y /= gridSize - 1 then
                getCenter x (y + 1) arr

            else
                getCenter x (y - 1) arr

        getLeft x y arr =
            if x /= 0 then
                getCenter (x - 1) y arr

            else
                getCenter 0 y arr

        getRight x y arr =
            if x /= gridSize - 1 then
                getCenter (x + 1) y arr

            else
                getCenter (x - 1) y arr

        uLap x y =
            ((getRight x y reactionArr).uValue + (getLeft x y reactionArr).uValue + (getUp x y reactionArr).uValue + (getDown x y reactionArr).uValue - 4 * (getCenter x y reactionArr).uValue) / (delta_h ^ 2)

        vLap x y =
            ((getRight x y reactionArr).vValue + (getLeft x y reactionArr).vValue + (getUp x y reactionArr).vValue + (getDown x y reactionArr).vValue - 4 * (getCenter x y reactionArr).vValue) / (delta_h ^ 2)

        next_u x y =
            (getCenter x y reactionArr).uValue + ((model.a * ((getCenter x y reactionArr).uValue - model.h)) + (model.b * (getCenter x y reactionArr).vValue - model.k) + diff_u * uLap x y) * delta_t

        next_v x y =
            (getCenter x y reactionArr).vValue + ((model.c * ((getCenter x y reactionArr).uValue - model.h)) + (model.d * (getCenter x y reactionArr).vValue - model.k) + diff_v * vLap x y) * delta_t

        nextVal r =
            rVal r.x r.y (next_u r.x r.y) (next_v r.x r.y)
    in
    List.map nextVal model.cells


indexToCoord : Int -> ( Int, Int )
indexToCoord i =
    ( modBy i gridSize, i // gridSize )


coordToIndex : ( Int, Int ) -> Int
coordToIndex ( x, y ) =
    x + y * gridSize



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 8000



-- Create a function for 2D noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 1.0, steps = 7, stepSize = 2.0, persistence = 2.0 } permTable
