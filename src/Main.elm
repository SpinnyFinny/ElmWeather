module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html, div, h1, img, span, text)
import Html.Attributes exposing (class, src)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, map4, string)
import Json.Decode.Pipeline exposing (hardcoded, required)



---- MODEL ----


type alias Model =
    { status : Status
    }


initialModel : Model
initialModel =
    { status = Loading }


init : ( Model, Cmd Msg )
init =
    ( initialModel, getCurrentWeather )


type alias OpenWeatherResponse =
    {}


currentForecastDecoder : Decoder CurrentForecast
currentForecastDecoder =
    Decode.map4 CurrentForecast
        (Decode.field "weather" <| Decode.index 0 <| Decode.field "icon" Decode.string)
        (Decode.at [ "main", "temp" ] Decode.float)
        (Decode.at [ "main", "temp_min" ] Decode.float)
        (Decode.at [ "main", "temp_max" ] Decode.float)



---- UPDATE ----


type Msg
    = GotCurrentWeather (Result Http.Error CurrentForecast)


type Status
    = Loaded CurrentForecast
    | Loading
    | Errored String


type alias CurrentForecast =
    { icon : String
    , currentTemp : Float
    , tempMin : Float
    , tempMax : Float
    }


getCurrentWeather : Cmd Msg
getCurrentWeather =
    Http.get
        { url = "https://api.openweathermap.org/data/2.5/weather?id=6090785&units=metric&appid=d2181649349acdfe3ca432ea262f0d7b"
        , expect = Http.expectJson GotCurrentWeather currentForecastDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentWeather (Ok forecast) ->
            ( { model | status = Loaded forecast }, Cmd.none )

        GotCurrentWeather (Err httpErr) ->
            ( { model | status = Errored "Leo fucked it up" }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.status of
        Loaded forecast ->
            -- viewCurrentWeather forecast model
            Element.layout [] <| getCurrentForecast forecast

        Loading ->
            Element.layout [] <|
                Element.column
                    [ centerX
                    , centerY
                    , Font.family
                        [ Font.external
                            { name = "Alata"
                            , url = "https://fonts.googleapis.com/css?family=Alata"
                            }
                        , Font.sansSerif
                        ]
                    , Font.size
                        40
                    ]
                    [ Element.text "Loading..." ]

        Errored err ->
            Html.text ("Error: " ++ err)


getCurrentForecast : CurrentForecast -> Element msg
getCurrentForecast forecast =
    Element.column
        [ centerX
        , centerY

        -- Background.gradient
        -- { angle = 0.5
        -- , steps =
        --     [ rgb 0 0 0
        --     , rgb 0 0 0
        --     , rgb 0 0 0
        --     , rgb 67 67 67
        --     ]
        -- }
        , Background.color
            (rgb 0 0 0)
        , Border.rounded 7
        , Font.color (rgb 1 1 1)
        , Font.family
            [ Font.external
                { name = "Alata"
                , url = "https://fonts.googleapis.com/css?family=Alata"
                }
            , Font.sansSerif
            ]
        , Element.padding 10
        ]
        [ Element.row [ width fill, spaceEvenly ]
            [ Input.button [ Border.rounded 7, Border.width 1, padding 5 ] { onPress = Nothing, label = Element.text "North Vancouver" }
            , Element.text "4:20"
            ]
        , Element.row [ centerX, centerY ]
            [ Element.image []
                { src = "http://openweathermap.org/img/w/" ++ forecast.icon ++ ".png", description = "" }
            ]
        , Element.row
            [ width fill, spaceEvenly ]
            [ Element.column []
                [ Element.text ("High: " ++ String.fromFloat forecast.tempMax ++ "℃")
                , Element.text ("Low: " ++ String.fromFloat forecast.tempMin ++ "℃")
                ]
            , Element.text
                (String.fromFloat forecast.currentTemp ++ "℃")
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
