module Main exposing (..)

import Browser
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


currentForcastDecoder : Decoder CurrentForcast
currentForcastDecoder =
    Decode.map4 CurrentForcast
        (Decode.field "weather" <| Decode.index 0 <| Decode.field "icon" Decode.string)
        (Decode.at [ "main", "temp" ] Decode.float)
        (Decode.at [ "main", "temp_min" ] Decode.float)
        (Decode.at [ "main", "temp_max" ] Decode.float)



---- UPDATE ----


type Msg
    = GotCurrentWeather (Result Http.Error CurrentForcast)


type Status
    = Loaded CurrentForcast
    | Loading
    | Errored String


type alias CurrentForcast =
    { icon : String
    , currentTemp : Float
    , tempMin : Float
    , tempMax : Float
    }


getCurrentWeather : Cmd Msg
getCurrentWeather =
    Http.get
        { url = "https://api.openweathermap.org/data/2.5/weather?id=6090785&units=metric&appid=d2181649349acdfe3ca432ea262f0d7b"
        , expect = Http.expectJson GotCurrentWeather currentForcastDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentWeather (Ok forcast) ->
            ( { model | status = Loaded forcast }, Cmd.none )

        GotCurrentWeather (Err httpErr) ->
            ( { model | status = Errored "Leo fucked it up" }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded forcast ->
                viewCurrentWeather forcast model

            Loading ->
                [ h1 [] [ text "Loading..." ] ]

            Errored err ->
                [ text ("Error: " ++ err) ]


viewCurrentWeather : CurrentForcast -> Model -> List (Html Msg)
viewCurrentWeather forcast model =
    [ div
        [ class "current-weather" ]
        [ h1 [] [ text "Current Weather in North Van" ]
        , img [ class "weather-thumb", src ("http://openweathermap.org/img/w/" ++ forcast.icon ++ ".png") ] []
        , div [ class "current-temp" ] [ text ("Current Temperture: " ++ String.fromFloat forcast.currentTemp ++ "℃") ]
        , div [ class "current-high" ] [ text ("High: " ++ String.fromFloat forcast.tempMax ++ "℃") ]
        , div [ class "current-low" ] [ text ("Low: " ++ String.fromFloat forcast.tempMin ++ "℃") ]
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
