module Ball exposing (main)

import Arc2d exposing (pointOn)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (Renderable, circle, rect, shapes, toHtml)
import Canvas.Settings exposing (fill)
import Color
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time exposing (Posix)


type alias Model =
    { location : Point
    , time : Float
    }


type alias Point =
    { x : Float
    , y : Float
    }


type Msg
    = AnimationFrame Posix


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


init : () -> ( Model, Cmd Msg )
init () =
    ( { location = { x = 0, y = 0 }, time = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        position =
            boxPosition model
    in
    case msg of
        AnimationFrame t ->
            ( { location = position, time = t |> Time.posixToMillis |> toFloat }, Cmd.none )


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
speed =
    2


armLength : Float
armLength =
    200


view : Model -> Html.Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( round w, round h )
            []
            [ -- shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ] ,
              renderItem model
            ]
        ]


boxPosition : Model -> Point
boxPosition model =
    { x = sin (speed * model.time / 1000) * armLength
    , y = cos (speed * model.time / 1000) * armLength
    }


renderItem : Model -> Renderable
renderItem model =
    let
        originx =
            w / 2

        originy =
            h / 2
    in
    shapes [ fill Color.darkOrange ]
        [ circle ( originx + model.location.x, originy + model.location.y ) size ]
