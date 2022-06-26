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

type alias Model = {
    x: Float,
    y: Float,
    time: Float
    }

type Msg = AnimationFrame Posix


main : Program () Model Msg
main =    
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


init : () -> (Model, Cmd Msg)
init () =
    ( {x=50,y=50,time=0}, Cmd.none )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnimationFrame t ->
            ( {x=0, y=0 , time=t |> Time.posixToMillis |> toFloat }, Cmd.none )


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
    renderItem model.time]
    ]


renderItem : Float -> Renderable
renderItem time =
    let
        x = sin(speed * time / 1000) * armLength
        y = cos(speed * time/ 1000) * armLength

        originx = w / 2
        originy = h / 2
    in
        shapes [ fill Color.darkOrange ] [ rect ( originx+x, originy+y ) size size ]

