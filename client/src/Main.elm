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
import RemoteData exposing (RemoteData(..))
import Station exposing (Station)


port receivePredictions : (D.Value -> msg) -> Sub msg


type alias Flags =
    {}


type Msg
    = ReceivedStations (Result Http.Error (List Station))
    | ReceivedPredictions D.Value
    | SearchTextChanged String


type alias Model =
    { searchText : String
    , stations : RemoteData Http.Error (List Station)
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

        ReceivedStations (Result.Err httpErr) ->
            ( { model
                | stations = Failure httpErr
              }
            , Cmd.none
            )

        ReceivedStations (Result.Ok stations) ->
            ( { model
                | stations = Success stations
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { searchText = ""
      , stations = Loading
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
        , expect = Http.expectJson ReceivedStations (D.list Station.decodeStation)
        }
    )


view : Model -> H.Html Msg
view model =
    El.layoutWith
        { options =
            [ El.focusStyle
                { borderColor = Just secondary
                , backgroundColor = Nothing
                , shadow =
                    Just
                        { offset = ( 2, 2 )
                        , blur = 10
                        , size = 0
                        , color = El.rgba255 0 0 0 0.5
                        }
                }
            ]
        }
        [ Background.color secondaryBackground
        , Font.color text
        ]
        (El.column
            [ El.width El.fill
            ]
            [ El.el
                [ El.paddingXY 0 16
                , El.width El.fill
                , Background.color primaryBackground
                ]
                (searchInput model.searchText)
            , El.row
                [ El.width El.fill
                , El.inFront <| El.column [] []
                ]
                [ El.column
                    [ El.centerX
                    ]
                    (model.stations
                        |> RemoteData.map (Station.searchStation model.searchText)
                        |> RemoteData.map (List.map stationRow)
                        |> RemoteData.withDefault []
                    )
                ]
            ]
        )


stationRow : Station -> El.Element Msg
stationRow station =
    El.el
        []
        (El.text <| station.name ++ " " ++ (Station.lineCodeDisplay station |> Maybe.withDefault ""))


searchInput : String -> El.Element Msg
searchInput searchText =
    El.column
        [ El.centerX
        ]
        [ Input.text
            [ Border.color primary
            , Background.color primary
            , borderShadow
            ]
            { label =
                Input.labelBelow
                    [ El.paddingEach { edges | left = 16, top = 4 }
                    ]
                    (El.text "Station Name")
            , onChange = SearchTextChanged
            , placeholder =
                Just <|
                    Input.placeholder
                        [ Font.color text
                        ]
                        (El.text "Search")
            , text = searchText
            }
        ]


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


borderShadow =
    Border.shadow
        { offset = ( 2, 2 )
        , blur = 10 -- box-shadow: 5px 5px 20px
        , size = 0
        , color = El.rgba255 0 0 0 0.5
        }


backgroundColor =
    El.rgba255 255 255 255 0


text =
    El.rgba255 255 255 255 0.8


primaryBackground =
    El.rgba255 34 153 84 1


primary =
    El.rgba255 22 160 133 1


secondary =
    El.rgba255 230 126 34 1


secondaryBackground =
    El.rgba255 235 152 78 1


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
