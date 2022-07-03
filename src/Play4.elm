module Play4 exposing (main)

-- Trying to implement Belousov-Zhabotinsky Reaction from Scientific_Computing_Simulations_and_Modeling

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
import List.Extra exposing (..)
import Random exposing (..)
import Random.Extra exposing (result)
import Simplex exposing (PermutationTable)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type alias Model =
    { seed : Random.Seed
    , count : Int
    , k : Float
    , f : Float
    , cells : List ReactionValue
    }


type Msg
    = AnimationFrame Posix


type alias ReactionValue =
    { x : Int
    , y : Int
    , uValue : Float
    , vValue : Float
    , gradient : Vector
    }


type alias Vector =
    { x : Float
    , y : Float
    }


rVal : a -> b -> c -> d -> Vector -> { x : a, y : b, uValue : c, vValue : d, gradient : Vector }
rVal =
    \x y u v g ->
        { x = x
        , y = y
        , uValue = u
        , vValue = v
        , gradient = g
        }


main : Program Float Model Msg
main =
    Browser.element
        { init = \floatSeed -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { seed = Random.initialSeed (floor (42 * 10000))
      , count = 0
      , f = 0.055
      , k = 0.062
      , cells = List.sortWith sortCells (initReactionValues gridSize)
      }
    , Cmd.none
    )


sortCells a b =
    case compare (coordToIndex ( a.x, a.y )) (coordToIndex ( b.x, b.y )) of
        EQ ->
            EQ

        GT ->
            GT

        LT ->
            LT


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame AnimationFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            if model.count < maxIter then
                ( iterateModel model
                    |> iterateModel
                    |> iterateModel
                    |> iterateModel
                    |> iterateModel
                , Cmd.none
                )

            else
                ( model, Cmd.none )


h : number
h =
    500


w : number
w =
    h


maxIter : Int
maxIter =
    1000000


gridSize : number
gridSize =
    51


cellSize : Float
cellSize =
    h / gridSize


delta_t : Float
delta_t =
    1


diff_u : Float
diff_u =
    0.1


diff_v : Float
diff_v =
    0.05


view : Model -> Html Msg
view model =
    Canvas.toHtml
        ( w, h )
        [ style "backgroundColor" "black"
        ]
        (drawItAll
            model
        )


iterateModel : Model -> Model
iterateModel model =
    let
        nextCells =
            nextVals model
    in
    { model | cells = nextCells, count = model.count + 1 }


drawItAll : Model -> List Renderable
drawItAll model =
    List.map (drawPieceItem model) model.cells


drawPieceItem : Model -> ReactionValue -> Renderable
drawPieceItem model r =
    let
        scaledValue =
            scaleReactionValsToColor r.vValue 0.05 0.3
    in
    shapes
        [ fill (Color.hsla 0 0.5 scaledValue 0.5) ]
        [ circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) (cellSize * abs scaledValue) ]



--[ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]
--[ circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) (cellSize / 1.9) ]
--[ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]


scaleReactionValsToColor val minVal maxVal =
    (val - minVal) / (maxVal - minVal)


nextVals : Model -> List ReactionValue
nextVals model =
    let
        reactionArr =
            fromList model.cells

        getCenter x y arr =
            Maybe.withDefault (rVal x y 0.0 0.0 (Vector 0 0)) (get (coordToIndex ( x, y )) arr)

        getUp x y arr =
            getCenter x (y - 1) arr

        getDown x y arr =
            getCenter x (y + 1) arr

        getLeft x y arr =
            getCenter (x - 1) y arr

        getRight x y arr =
            getCenter (x + 1) y arr

        uLap x y =
            (getRight x y reactionArr).uValue
                + (getLeft x y reactionArr).uValue
                + (getUp x y reactionArr).uValue
                + (getDown x y reactionArr).uValue
                - (4 * (getCenter x y reactionArr).uValue)

        vLap x y =
            (getRight x y reactionArr).vValue
                + (getLeft x y reactionArr).vValue
                + (getUp x y reactionArr).vValue
                + (getDown x y reactionArr).vValue
                - (4 * (getCenter x y reactionArr).vValue)

        gradient x y =
            Vector
                ((getRight x y reactionArr).vValue
                    - (getLeft x y reactionArr).vValue
                )
                ((getUp x y reactionArr).vValue
                    - (getDown x y reactionArr).vValue
                )

        uvv x y =
            (getCenter x y reactionArr).uValue
                * (getCenter x y reactionArr).vValue
                * (getCenter x y reactionArr).vValue

        next_u x y =
            ((diff_u * uLap x y)
                - uvv x y
                + (model.f * (1 - (getCenter x y reactionArr).uValue))
            )
                * delta_t

        next_v x y =
            ((diff_v * vLap x y)
                + uvv x y
                - ((model.f + model.k)
                    * (getCenter x y reactionArr).vValue
                  )
            )
                * delta_t

        nextVal r =
            rVal r.x r.y (r.uValue + next_u r.x r.y) (r.vValue + next_v r.x r.y) (gradient r.x r.y)

        -- Set the right gradient value here.
    in
    List.map nextVal model.cells


indexToCoord : Int -> ( Int, Int )
indexToCoord i =
    ( modBy i gridSize, i // gridSize )


coordToIndex : ( Int, Int ) -> Int
coordToIndex ( x, y ) =
    modBy (gridSize * gridSize) (x + (y * gridSize))


perpendicular : Vector -> Vector
perpendicular v =
    Vector -v.y v.x


initReactionValues : Int -> List ReactionValue
initReactionValues n =
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> noiseSeeding x y :: result)
        []


noiseSeeding : Int -> Int -> ReactionValue
noiseSeeding x y =
    rVal x y (2.0 * noise (toFloat x) (toFloat y)) (0.3 * noise (toFloat x) (toFloat y)) (Vector 0 0)


seedCorner : Int -> Int -> ReactionValue
seedCorner x y =
    if x < 10 && y < 10 then
        rVal x y (Tuple.first defaultReactionValue) (Tuple.second defaultReactionValue) (Vector 0 0)

    else
        rVal x y 1 0 (Vector 0 0)


seedMiddle : Int -> Int -> ReactionValue
seedMiddle x y =
    let
        thickness =
            14

        middle =
            floor (gridSize / 2)
    in
    if x < (middle + thickness) && x > (middle - thickness) && y < (middle + thickness) && y > (middle - thickness) then
        rVal x y (Tuple.first defaultReactionValue) (Tuple.second defaultReactionValue) (Vector 0 0)

    else
        rVal x y 1.0 0.0 (Vector 0 0)


defaultReactionValue =
    ( 0.5, 0.25 )



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 10



-- Create a function for 2D noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 1, steps = 4, stepSize = 3.0, persistence = 2.0 } permTable
