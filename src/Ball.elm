module Ball exposing (main)


import Html
import Browser.Events exposing (onAnimationFrame)
import Time exposing (Posix)
import Browser
import Html exposing (div)
import Html.Attributes exposing (style)
import Canvas exposing (toHtml)
import Html.Events exposing (onClick)
import Canvas exposing (shapes)
import Canvas.Settings exposing (fill)
import Color
import Canvas exposing (rect)
import Canvas exposing (Renderable)
import Arc2d exposing (pointOn)

type alias Model = {
    location : Point,
    time: Float
    }

type alias Point = {
    x : Float,
    y : Float
    }


type Msg = AnimationFrame Posix


main : Program () Model Msg
main =    
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


init : () -> (Model, Cmd Msg)
init () =
    ( { location={x = 0, y = 0}, time=0}, Cmd.none )



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let position = boxPosition model
    in
    case msg of
        AnimationFrame t ->
            ( {location = position, time=t |> Time.posixToMillis |> toFloat }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame AnimationFrame


h : Float
h =
    1000


w : Float
w =
    1000

size : Float
size =
    50

speed : Float
speed = 1

armLength : Float
armLength =
    100


view : Model -> Html.Html Msg
view model = 
        div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
    [Canvas.toHtml 
    ( round w , round h )
    []
    [shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ] ,
    renderItem model]
    ]

boxPosition : Model -> Point
boxPosition model =

    {  x= sin(speed * model.time / 1000) * armLength, 
    y = cos(speed * model.time / 1000) * armLength }

renderItem : Model -> Renderable
renderItem model =
    let
        originx = w / 2
        originy = h / 2
    in
        shapes [ fill Color.darkOrange ] [ rect ( originx+model.location.x, originy+model.location.y ) size size ]

