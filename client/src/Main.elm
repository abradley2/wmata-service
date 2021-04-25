port module Main exposing (..)

import Browser exposing (element)
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
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
    | SearchTextChanged String


type alias Model =
    { searchText : String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchTextChanged searchText ->
            ( { model
                | searchText = searchText
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { searchText = ""
      }
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
    El.layoutWith
        { options =
            [ El.focusStyle
                { borderColor = Just primary
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Background.color backgroundColor
        , Font.color text
        ]
        (El.column
            [ El.width El.fill
            ]
            [ El.row
                [ El.centerX
                , El.paddingXY 0 16
                ]
                [ Input.text
                    [ Background.color overlay1
                    , Border.color overlay3
                    , Border.rounded 99999
                    ]
                    { label =
                        Input.labelBelow
                            [ El.paddingEach { edges | left = 16, top = 4 }

                            ]
                        <|
                            El.text "hi"
                    , onChange = SearchTextChanged
                    , placeholder = Nothing
                    , text = model.searchText
                    }
                ]
            ]
        )


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


backgroundColor =
    El.rgba255 18 18 18 1


overlay1 =
    El.rgba255 255 255 255 0.1


overlay2 =
    El.rgba255 255 255 255 0.2


overlay3 =
    El.rgba255 255 255 255 0.3


text =
    El.rgba 255 255 255 0.8


primary =
    El.rgba 255 165 0 1

edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
