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
import Html.Events exposing (onClick)
import List.Extra exposing (..)
import Random exposing (..)
import Random.Extra exposing (result)
import Simplex exposing (PermutationTable)
import SingleSlider exposing (..)
import Time exposing (Posix)
import Vector2d exposing (Vector2d)


type alias Model =
    { seed : Random.Seed
    , count : Int
    , k_reaction : Float
    , f_reaction : Float
    , cells : List ReactionValue
    , floaters : List Vector
    , drawField : Bool
    , f_slider : SingleSlider.SingleSlider Msg
    , floater_speed_slider : SingleSlider.SingleSlider Msg
    , floater_speed : Float
    }


type Msg
    = AnimationFrame Posix
    | SwapMode
    | FSliderChange Float
    | FloaterSpeedChange Float


type alias ReactionValue =
    { x : Int
    , y : Int
    , uValue : Float
    , vValue : Float
    , gradient : Vector
    , prevuValue : Float
    , prevvValue : Float
    }


type alias Vector =
    { x : Float
    , y : Float
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
    let
        f_valueFormatter =
            \value _ -> "F: " ++ String.fromFloat value

        floater_speed_valueFormatter =
            \value _ -> "Floater speed: " ++ String.fromFloat value

        noFormat =
            \value -> ""

        initialFloaterSpeed =
            5

        initial_f_value =
            0.023
    in
    ( { seed = Random.initialSeed (floor (42 * 10000))
      , count = 0
      , f_reaction = initial_f_value
      , k_reaction = 0.055
      , cells = List.sortWith sortCells (initReactionValues gridSize)
      , floaters = initFloaterRandom gridSize
      , drawField = False
      , floater_speed = initialFloaterSpeed
      , f_slider =
            SingleSlider.init
                { min = 0.02
                , max = 0.055
                , value = initial_f_value
                , step = 0.001
                , onChange = FSliderChange
                }
                |> SingleSlider.withValueFormatter f_valueFormatter
                |> SingleSlider.withMinFormatter noFormat
                |> SingleSlider.withMaxFormatter noFormat
      , floater_speed_slider =
            SingleSlider.init
                { min = 0.1
                , max = 100.0
                , value = initialFloaterSpeed
                , step = 0.1
                , onChange = FloaterSpeedChange
                }
                |> SingleSlider.withValueFormatter floater_speed_valueFormatter
                |> SingleSlider.withMinFormatter noFormat
                |> SingleSlider.withMaxFormatter noFormat
      }
    , Cmd.none
    )


defaultReactionValue =
    ( 0.5, 0.25 )


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
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        SwapMode ->
            ( { model | drawField = not model.drawField }
            , Cmd.none
            )

        FSliderChange sliderValue ->
            let
                newSlider =
                    SingleSlider.update sliderValue model.f_slider
            in
            ( { model | f_slider = newSlider, f_reaction = sliderValue }, Cmd.none )

        FloaterSpeedChange sliderValue ->
            let
                newSlider =
                    SingleSlider.update sliderValue model.floater_speed_slider
            in
            ( { model | floater_speed_slider = newSlider, floater_speed = sliderValue }, Cmd.none )


h : number
h =
    1000


w : number
w =
    h


maxIter : Int
maxIter =
    1000000


gridSize : number
gridSize =
    45


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


getCenter x y arr =
    Maybe.withDefault (rVal x y 0.0 0.0 (Vector 0 0) 0 0) (get (coordToIndex ( x, y )) arr)


view : Model -> Html Msg
view model =
    div []
        [ Canvas.toHtml
            ( w, h )
            [ style "backgroundColor"
                "black"
            , onClick
                SwapMode
            ]
            (drawItAll
                model
            )
        , div [] [ SingleSlider.view model.f_slider ]
        , div [] [ SingleSlider.view model.floater_speed_slider ]
        ]


iterateModel : Model -> Model
iterateModel model =
    let
        nextCells =
            nextVals model

        nextFloaterCalc =
            nextFloaters model
    in
    { model | cells = nextCells, count = model.count + 1, floaters = nextFloaterCalc }


drawItAll : Model -> List Renderable
drawItAll model =
    let
        fieldCircles =
            List.map (drawReactionCircles model) model.cells

        debugLines =
            List.map (drawPerpendicularLines model) model.cells

        floaters =
            List.map (drawFloater model) model.floaters

        reset =
            rect ( 0.0, 0.0 ) h h
    in
    if model.drawField then
        shapes
            [ fill (Color.hsla 0.5 0.2 0.2 0.005) ]
            [ reset ]
            :: fieldCircles
            ++ floaters

    else
        shapes
            [ fill (Color.hsla 0.5 0.2 0.2 0.005) ]
            [ reset ]
            :: floaters



--++ fieldCircles
--    floaters
-- :: circs
--++ debugLines
--++ debugLines
--List.map (drawReactionCircles model) model.cells
--List.map (drawPerpendicularLines model) model.cells


drawReactionCircles : Model -> ReactionValue -> Renderable
drawReactionCircles model r =
    let
        scaledValue =
            scaleReactionValsToColor r.vValue 0.05 0.3
    in
    shapes
        [ fill (Color.hsla 0.2 0.5 0.0 scaledValue) ]
        [ circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) (cellSize * abs scaledValue / 2)
        ]



--[ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]
--[ circle ( toFloat r.x * cellSize, toFloat r.y * cellSize ) (cellSize / 1.9) ]
--[ rect ( toFloat r.x * cellSize, toFloat r.y * cellSize ) cellSize cellSize ]


sineMod model =
    0.5 + sin (toFloat model.count / 200) / 2


floaterSizeMod : Model -> Float
floaterSizeMod model =
    10



--0.5 + abs (sin (toFloat model.count / 10))


clampMod value min max =
    -- Drawing circles with negative radius causes js to stop.
    if value < min then
        min

    else if value > max then
        max

    else
        value


drawFloater : Model -> Vector -> Renderable
drawFloater model floater =
    shapes
        [ fill (Color.hsla 1 1 1 0.9) ]
        [ circle ( floater.x, floater.y ) (clampMod (floaterSizeMod model) 0.1 2)
        ]


drawPerpendicularLines : Model -> ReactionValue -> Renderable
drawPerpendicularLines model r =
    let
        origin =
            { x = toFloat r.x * cellSize, y = toFloat r.y * cellSize }

        strength =
            5

        perpGrad =
            perpendicular r.gradient
    in
    shapes
        [ stroke Color.black
        , lineWidth 1
        ]
        [ path ( origin.x, origin.y ) [ lineTo ( origin.x + (perpGrad.x * cellSize * strength), origin.y + (perpGrad.y * cellSize * strength) ) ] ]


scaleReactionValsToColor val minVal maxVal =
    (val - minVal) / (maxVal - minVal)


nextVals : Model -> List ReactionValue
nextVals model =
    let
        reactionArr =
            fromList model.cells

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

        scaledVal x y r =
            scaleReactionValsToColor r 0.01 0.1

        gradient x y =
            Vector
                ((scaledVal x y (getRight x y reactionArr).vValue
                    - scaledVal x y (getLeft x y reactionArr).vValue
                 )
                    / 2
                )
                ((scaledVal x y (getDown x y reactionArr).vValue
                    - scaledVal x y (getUp x y reactionArr).vValue
                 )
                    / 2
                )

        uvv x y =
            (getCenter x y reactionArr).uValue
                * (getCenter x y reactionArr).vValue
                * (getCenter x y reactionArr).vValue

        next_u x y =
            ((diff_u * uLap x y)
                - uvv x y
                + (model.f_reaction * (1 - (getCenter x y reactionArr).uValue))
            )
                * delta_t

        next_v x y =
            ((diff_v * vLap x y)
                + uvv x y
                - ((model.f_reaction + model.k_reaction)
                    * (getCenter x y reactionArr).vValue
                  )
            )
                * delta_t

        nextVal r =
            rVal r.x r.y (r.uValue + next_u r.x r.y) (r.vValue + next_v r.x r.y) (gradient r.x r.y) r.uValue r.vValue

        -- Set the right gradient value here.
    in
    List.map nextVal model.cells


nextFloaters : Model -> List Vector
nextFloaters model =
    List.map (nextFloater model) model.floaters


nextFloater : Model -> Vector -> Vector
nextFloater model floater =
    let
        middleAdjust =
            0.5

        invert v =
            Vector
                (v.x * -1)
                (v.y * -1)

        getReactionOfLocation location =
            getCenter (floor (middleAdjust + (location.x / cellSize))) (floor (middleAdjust + (location.y / cellSize))) (fromList model.cells)

        getGradient location =
            (getReactionOfLocation location).gradient

        perpVec location =
            perpendicular (getGradient location)

        perpendicularMovement =
            normalize (invert (perpVec floater))

        normalize v =
            Vector
                (v.x / (0.0001 + sqrt ((v.x * v.x) + (v.y * v.y))))
                -- ( the 0.0001 is to prevent divide by zero )
                (v.y / (0.0001 + sqrt ((v.x * v.x) + (v.y * v.y))))

        floaterSpeed =
            model.floater_speed

        stayInsideBorders v =
            Vector
                (if v.x < 0 then
                    h

                 else if v.x >= h then
                    0

                 else
                    v.x
                )
                (if v.y < 0 then
                    h

                 else if v.y > h then
                    0

                 else
                    v.y
                )
    in
    stayInsideBorders (Vector (floater.x + (floaterSpeed * perpendicularMovement.x)) (floater.y + (floaterSpeed * perpendicularMovement.y)))


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
        (\( x, y ) result -> seedMiddle x y :: result)
        []


initFloater : Int -> List Vector
initFloater n =
    Grid.fold2d
        { rows = n, cols = n }
        (\( x, y ) result -> Vector ((toFloat x + 0.5) * 2 * cellSize) ((toFloat y + 0.5) * 2 * cellSize) :: result)
        []


initFloaterRandom : Int -> List Vector
initFloaterRandom n =
    let
        noiseStrength =
            0

        numScale =
            0.5
    in
    Grid.fold2d
        { rows = floor (toFloat n * numScale), cols = floor (toFloat n * numScale) }
        (\( x, y ) result ->
            Vector (cellSize / numScale * (toFloat x + (noiseStrength * noise (toFloat x) (toFloat y))))
                (cellSize / numScale * (toFloat y + (noiseStrength * noise (toFloat x) (toFloat y))))
                :: result
        )
        []


noiseSeeding : Int -> Int -> ReactionValue
noiseSeeding x y =
    rVal x y (2.0 * noise (toFloat x) (toFloat y)) (0.3 * noise (toFloat x) (toFloat y)) (Vector 0 0) 0 0


seedCorner : Int -> Int -> ReactionValue
seedCorner x y =
    if x < 10 && y < 10 then
        rVal x y (Tuple.first defaultReactionValue) (Tuple.second defaultReactionValue) (Vector 0 0) 0 0

    else
        rVal x y 1 0 (Vector 0 0) 0 0


seedMiddle : Int -> Int -> ReactionValue
seedMiddle x y =
    let
        thickness =
            2

        middle =
            floor (gridSize / 2)
    in
    if x < (middle + thickness) && x > (middle - thickness) && y < (middle + thickness) && y > (middle - thickness) then
        rVal x y (Tuple.first defaultReactionValue) (Tuple.second defaultReactionValue) (Vector 0 0) 0 0

    else
        rVal x y 1.0 0.0 (Vector 0 0) 0 0



--Create a permutation table, using 42 as the seed


permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 23



-- Create a function for 2D noise


noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 1, steps = 4, stepSize = 3.0, persistence = 2.0 } permTable



-- yes yes I know.


rVal : a -> b -> c -> d -> Vector -> Float -> Float -> { x : a, y : b, uValue : c, vValue : d, gradient : Vector, prevuValue : Float, prevvValue : Float }
rVal =
    \x y u v g pu pv ->
        { x = x
        , y = y
        , uValue = u
        , vValue = v
        , gradient = g
        , prevuValue = pu
        , prevvValue = pv
        }
