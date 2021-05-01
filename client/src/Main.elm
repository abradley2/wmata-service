port module Main exposing (..)

import Browser exposing (element)
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import Html as H
import Html.Attributes exposing (attribute)
import Http
import Json.Decode as D
import Maybe.Extra as MaybeX
import Platform exposing (Program)
import Random exposing (initialSeed)
import RemoteData exposing (RemoteData(..))
import Station exposing (Station)
import Task
import Time exposing (Posix)
import UUID exposing (Seeds, UUID)


port receivePredictions : (D.Value -> msg) -> Sub msg


type alias Flags =
    { decoded : Bool
    , now : Int
    , seeds : Seeds
    }


defaultFlags : Flags
defaultFlags =
    { decoded = False
    , now = 0
    , seeds =
        Seeds
            (Random.initialSeed 0)
            (Random.initialSeed 0)
            (Random.initialSeed 0)
            (Random.initialSeed 0)
    }


flagsDecoder : D.Decoder Flags
flagsDecoder =
    D.map3
        Flags
        (D.succeed True)
        (D.at [ "now" ] D.int)
        (D.at [ "seeds" ] <|
            D.map4 Seeds
                (D.at [ "0" ] D.int |> D.map Random.initialSeed)
                (D.at [ "1" ] D.int |> D.map Random.initialSeed)
                (D.at [ "2" ] D.int |> D.map Random.initialSeed)
                (D.at [ "3" ] D.int |> D.map Random.initialSeed)
        )


type Msg
    = ReceivedStations (Result Http.Error (List Station))
    | ReceivedTime Posix
    | ReceivedPredictions D.Value
    | SearchTextChanged String
    | StationSelected Station


type alias Model =
    { appInitialized : Result String ()
    , currentTime : Posix
    , clientId : UUID
    , searchText : String
    , stations : RemoteData Http.Error (List Station)
    , selectedStation : Maybe ( Station, Maybe Station )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StationSelected station ->
            let
                coStation =
                    MaybeX.andThen2
                        (\code ->
                            List.filter (.code >> (==) code) >> List.head
                        )
                        station.coStations.coStation1
                        (RemoteData.toMaybe model.stations)
            in
            ( { model
                | selectedStation = Just ( station, coStation )
              }
            , Cmd.none
            )

        SearchTextChanged searchText ->
            ( { model
                | searchText = searchText
              }
            , Cmd.none
            )

        ReceivedTime time ->
            ( { model
                | currentTime = time
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


init : D.Value -> ( Model, Cmd Msg )
init flagsJson =
    let
        flagsResult =
            D.decodeValue
                flagsDecoder
                flagsJson
                |> Result.mapError D.errorToString

        flags =
            Result.withDefault defaultFlags flagsResult

        ( clientId, nextSeeds ) =
            UUID.step flags.seeds
    in
    ( { appInitialized = flagsResult |> Result.map (always ())
      , clientId = clientId
      , currentTime = Time.millisToPosix flags.now
      , searchText = ""
      , stations = Loading
      , selectedStation = Nothing
      }
    , Cmd.batch
        [ Http.request
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
        ]
    )


view : Model -> H.Html Msg
view model =
    El.layoutWith
        { options =
            [ El.focusStyle
                { borderColor = Just foregroundText
                , backgroundColor = Just primary
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
        [ Font.color text
        , El.width El.fill
        ]
        (El.column
            [ El.width El.fill
            ]
            [ El.el
                [ El.paddingXY 0 16
                , El.width El.fill
                , Border.widthEach { edges | bottom = 3 }
                , Border.color primary
                ]
                (searchInput model.searchText)
            , El.column
                [ El.centerX
                , El.paddingXY 0 16
                ]
                (model.stations
                    |> RemoteData.map (Station.searchStation model.searchText)
                    |> RemoteData.map (List.map stationEl)
                    |> RemoteData.withDefault []
                )
            ]
        )


stationEl : Station -> El.Element Msg
stationEl station =
    let
        label =
            El.paragraph [] <|
                [ El.text <|
                    station.name
                        ++ " "
                        ++ (Station.lineCodeDisplay station |> Maybe.withDefault "")
                ]
    in
    El.el
        [ El.width (El.px 320)
        , El.paddingXY 0 8
        ]
        (Input.button
            [ El.width El.fill
            , Background.color primaryLight
            , Font.color (El.rgba255 255 255 255 1)
            , Font.semiBold
            , Border.rounded 8
            , El.paddingXY 16 16
            ]
            { onPress = Just <| StationSelected station
            , label = label
            }
        )


searchInput : String -> El.Element Msg
searchInput searchText =
    Input.search
        [ Border.color primary
        , Background.color primaryLight
        , borderShadow
        , Font.color foregroundText
        , El.width (El.px 320)
        , El.centerX
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
                    [ Font.color foregroundText
                    ]
                    (El.text "Search")
        , text = searchText
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receivePredictions ReceivedPredictions
        , Time.every 1000 ReceivedTime
        ]


main : Program D.Value Model Msg
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
    El.rgba255 0 0 0 0.5


foregroundText =
    El.rgba255 255 255 255 0.8


primaryBackground =
    El.rgba255 255 255 255 1


primary =
    El.rgba255 233 20 54 1


primaryLight =
    El.rgba255 233 20 54 0.8


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }
