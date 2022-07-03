module Play3 exposing (main)

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
    }


rVal : a -> b -> c -> d -> { x : a, y : b, uValue : c, vValue : d }
rVal =
    \x y u v ->
        { x = x
        , y = y
        , uValue = u
        , vValue = v
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
      , f = 0.03
      , k = 0.055
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
    85


cellSize : Float
cellSize =
    h / gridSize


delta_h : Float
delta_h =
    1


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


drawItAll : Model -> List Renderable
drawItAll model =
    List.map (drawPieceItem model) model.cells


drawPieceItem : Model -> ReactionValue -> Renderable
drawPieceItem model r =
    let
        scaledValue =
            scaleReactionValsToColor r.vValue 0 0.4
    in
    shapes
        [ fill (Color.hsla (-0.5 + scaledValue) 0.5 0.5 1) ]
        [ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]



--[ circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) (cellSize / 1.9) ]
--[ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]


scaleReactionValsToColor val minVal maxVal =
    (val - minVal) / (maxVal - minVal)


slowScalingVals val min max model =
    let
        maybMaxVal =
            List.Extra.maximumBy .vValue model.cells

        maybMinVal =
            List.Extra.minimumBy .vValue model.cells

        maxVal =
            checkMaybeVal maybMaxVal

        minVal =
            checkMaybeVal maybMinVal
    in
    scaleReactionValsToColor val minVal.vValue maxVal.vValue


checkMaybeVal : Maybe ReactionValue -> ReactionValue
checkMaybeVal maybeVal =
    case maybeVal of
        Nothing ->
            rVal 0 0 0 0

        Just val ->
            val


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
    { model | cells = nextCells, count = model.count + 1 }


nextVals : Model -> List ReactionValue
nextVals model =
    let
        reactionArr =
            fromList model.cells

        getCenter x y arr =
            Maybe.withDefault (rVal x y 0.0 0.0) (get (coordToIndex ( x, y )) arr)

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
            rVal r.x r.y (r.uValue + next_u r.x r.y) (r.vValue + next_v r.x r.y)
    in
    List.map nextVal model.cells


indexToCoord : Int -> ( Int, Int )
indexToCoord i =
    ( modBy i gridSize, i // gridSize )


coordToIndex : ( Int, Int ) -> Int
coordToIndex ( x, y ) =
    x + (y * gridSize)


initReactionValues : Int -> List ReactionValue
initReactionValues n =
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> seedMiddle x y :: result)
        []


noiseSeeding : Int -> Int -> ReactionValue
noiseSeeding x y =
    rVal x y (2 * noise (toFloat x) (toFloat y)) (3 * noise (toFloat x) (toFloat y))


seedCorner : Int -> Int -> ReactionValue
seedCorner x y =
    if x < 10 && y < 10 then
        rVal x y (Tuple.first defaultReactionValue) (Tuple.second defaultReactionValue)

    else
        rVal x y 1 0


seedMiddle : Int -> Int -> ReactionValue
seedMiddle x y =
    let
        thickness =
            14

        middle =
            floor (gridSize / 2)
    in
    if x < (middle + thickness) && x > (middle - thickness) && y < (middle + thickness) && y > (middle - thickness) then
        rVal x y (Tuple.first defaultReactionValue) (Tuple.second defaultReactionValue)

    else
        rVal x y 1.0 0.0


defaultReactionValue =
    ( 0.5, 0.25 )


seedStick : Int -> Int -> ReactionValue
seedStick x y =
    let
        thickness =
            10.0

        middle =
            gridSize / 2
    in
    if toFloat x < (middle + thickness + (3 / toFloat y)) && toFloat x > (middle - thickness) && toFloat y < (middle + thickness) then
        rVal x y 0.61 0.5

    else
        rVal x y 0.01 0.05


initZeroReactionValues : Int -> List ReactionValue
initZeroReactionValues n =
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> rVal x y 0 0 :: result)
        []



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 43



-- Create a function for 2D noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 1, steps = 4, stepSize = 3.0, persistence = 2.0 } permTable
