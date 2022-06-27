port module Ball exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Color exposing (..)
import Hash exposing (Hash)
import Html exposing (div)
import Html.Attributes exposing (style)
import Json.Decode as D
import Json.Encode as E
import Random exposing (..)
import String exposing (toInt)
import Time exposing (Posix)



-- Javascript interop, trigger fxpreview


port fxpreview : String -> Cmd msg



-- Used for getting fxhash settings from javascript


type alias Flags =
    { fxhash : String
    , isFxpreview : Bool
    }


decoder : D.Decoder Flags
decoder =
    D.map2 Flags
        (D.field "fxhash" D.string)
        (D.field "isFxpreview" D.bool)


type alias Model =
    { location : Point
    , count : Float
    , flags : Flags
    , seed : Seed
    }


type Msg
    = AnimationFrame Posix
    | NewRandomNumber


type alias Point =
    { x : Float
    , y : Float
    }


type alias PieceSettings =
    { color : Color
    , radius : Float
    , speed : Float
    , direction : Float
    }


main : Program E.Value Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


seed0 : Model -> Seed
seed0 model =
    Random.initialSeed (Maybe.withDefault 0 (String.toInt (Hash.toString (Hash.fromString model.flags.fxhash))))


randomNum : Generator Float
randomNum =
    Random.float 0 1


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( -- Init the model with the flags from fxhash which is stored in the head of index.html
      case D.decodeValue decoder flags of
        Ok decodedFlags ->
            { location = { x = 0, y = 0 }
            , count = 0.0
            , flags = decodedFlags
            , seed = Random.initialSeed (Maybe.withDefault 0 (String.toInt (Hash.toString (Hash.fromString decodedFlags.fxhash))))
            }

        Err _ ->
            { location = { x = 0.0, y = 0.0 }
            , count = 0.0
            , flags = { fxhash = "", isFxpreview = False }
            , seed = Random.initialSeed (Maybe.withDefault 0 (String.toInt (Hash.toString (Hash.fromString ""))))
            }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            if model.count == 100 then
                -- TODO, how can this be DRY and clean?
                ( { model
                    | count = model.count + 1
                    , location = nextLocation model (pieceSettings model)
                  }
                , fxpreview "FxPreview"
                  -- Decieds when FxHash captures the preview image of the piece.
                )

            else
                ( { model
                    | count = model.count + 1
                    , location = nextLocation model (pieceSettings model)
                  }
                , Cmd.none
                )

        NewRandomNumber ->
            let
                ( newValue, newSeed ) =
                    Random.step randomNum model.seed

                --a = Debug.log "generated a new value" newValue
            in
            ( { model | seed = newSeed }, Cmd.none )


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


pieceSettings : Model -> PieceSettings
pieceSettings model =
    { color = Color.orange
    , radius = Tuple.first (Random.step randomNum model.seed)
    , speed = 10
    , direction = 0
    }


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
            [ -- shapes [ fill Color.white ] [ rect ( 0, 0 ) w h ]
              renderItem model
            ]
        ]


armlength : Float -> Float
armlength count =
    count / 5


nextLocation : Model -> PieceSettings -> Point
nextLocation model settings =
    { x = sin ((pieceSettings model).radius * model.count / 60) * armlength model.count
    , y = cos (settings.speed * model.count / 60) * armlength model.count
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
