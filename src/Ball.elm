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
    , pieceSettings : PieceSettings
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
    , armLength : Float
    , speed : Float
    , size : Float
    }


main : Program E.Value Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


zeroToOnegenerator : Generator Float
zeroToOnegenerator =
    Random.float 0 1


armLengthgenerator : Generator Float
armLengthgenerator =
    Random.float 0.1 0.2


speedGenerator : Generator Float
speedGenerator =
    Random.float 5 100


sizeGenerator : Generator Float
sizeGenerator =
    Random.float 1 5


init : E.Value -> ( Model, Cmd Msg )
init flags =
    ( -- Init the model with the flags from fxhash which is stored in the head of index.html
      case D.decodeValue decoder flags of
        Ok decodedFlags ->
            let
                initialSeed =
                    Random.initialSeed (Maybe.withDefault 0 (String.toInt (Hash.toString (Hash.fromString decodedFlags.fxhash))))

                -- TODO is this way of generating unque random numbers with known seeds the only way?
                -- Note that seeds are sort of daisyChained, this allows different deterministic random number to be generated
                ( armLength, seed1 ) =
                    Random.step armLengthgenerator initialSeed

                ( speed, seed2 ) =
                    Random.step speedGenerator seed1

                ( size, seed3 ) =
                    Random.step sizeGenerator seed2

                pieceSettings =
                    { color = Color.orange
                    , armLength = armLength
                    , speed = speed
                    , size = size
                    }
            in
            { location = { x = 0, y = 0 }
            , count = 0.0
            , flags = decodedFlags
            , seed = seed3
            , pieceSettings = pieceSettings
            }

        Err _ ->
            { location = { x = 0.0, y = 0.0 }
            , count = 0.0
            , flags = { fxhash = "", isFxpreview = False }
            , seed = Random.initialSeed (Maybe.withDefault 0 (String.toInt (Hash.toString (Hash.fromString ""))))
            , pieceSettings =
                { color = Color.orange
                , armLength = 0
                , speed = 0
                , size = 0
                }
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
                    , location = nextLocation model
                  }
                , fxpreview "FxPreview"
                  -- Decieds when FxHash captures the preview image of the piece.
                )

            else
                ( { model
                    | count = model.count + 1
                    , location = nextLocation model
                  }
                , Cmd.none
                )

        NewRandomNumber ->
            let
                ( _, newSeed ) =
                    Random.step zeroToOnegenerator model.seed

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


nextLocation : Model -> Point
nextLocation model =
    { x = sin (model.pieceSettings.speed * model.count / 60) * model.pieceSettings.armLength * model.count
    , y = cos (model.pieceSettings.speed * model.count / 60) * model.pieceSettings.armLength * model.count
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
        [ circle ( originx + model.location.x, originy + model.location.y ) model.pieceSettings.size ]
