module Ball exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color exposing (..)
import Html exposing (div)
import Html.Attributes exposing (style)
import Time exposing (Posix)


type alias Model =
    { location : Point
    , count : Float
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
    ( { location = { x = 0, y = 0 }, count = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame t ->
            ( { location = boxPosition model, count = model.count + 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    onAnimationFrame AnimationFrame


h : Float
h =
    1000


w : Float
w =
    1000


size : Float
size =
    5


speed : Float
speed =
    10


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


armlength : Float -> Float
armlength count =
    count / 5


boxPosition : Model -> Point
boxPosition model =
    { x = sin (speed * model.count / 60) * armlength model.count
    , y = cos (speed * model.count / 60) * armlength model.count
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
