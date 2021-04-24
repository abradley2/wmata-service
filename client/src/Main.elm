port module Main exposing (..)

import Browser exposing (element)
import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Events as Ev
import Html as H
import Http
import Json.Decode as D
import Platform exposing (Program)


port receivePredictions : (D.Value -> msg) -> Sub msg


type alias Flags =
    {}


type Msg
    = ReceivedStations (Result Http.Error D.Value)
    | ReceivedPredictions D.Value


type alias Model =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}
    , Http.request
        { method = "GET"
        , body = Http.emptyBody
        , timeout = Just <| 1000 * 5
        , headers =
            [ Http.header "Accept" "application/json"
            ]
        , tracker = Nothing
        , url = "/api/stations"
        , expect = Http.expectJson ReceivedStations D.value
        }
    )


view : Model -> H.Html Msg
view model =
    El.layout [] (El.text "Hello There")


subscriptions : Model -> Sub Msg
subscriptions _ =
    receivePredictions ReceivedPredictions


main : Program Flags Model Msg
main =
    element
        { update = update
        , init = init
        , view = view
        , subscriptions = subscriptions
        }
