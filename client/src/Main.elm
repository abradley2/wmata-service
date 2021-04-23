module Main exposing (..)

import Browser exposing (element)
import Element as El
import Element.Background as Bg
import Element.Border as Bd
import Element.Events as Ev
import Html as H
import Platform exposing (Program)


type alias Flags =
    {}


type Msg
    = NoOp


type alias Model =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {}, Cmd.none )


view : Model -> H.Html Msg
view model =
    El.layout [] (El.text "Hello There")


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Flags Model Msg
main =
    element
        { update = update
        , init = init
        , view = view
        , subscriptions = subscriptions
        }
