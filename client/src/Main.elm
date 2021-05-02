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
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as E
import Maybe.Extra as MaybeX
import Platform exposing (Program)
import Prediction exposing (Prediction)
import Random
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
    | TimeStampedPredictions (List Prediction) Posix
    | SearchTextChanged String
    | StationSelected Station
    | LoggedError (Result Http.Error ())


type alias Model =
    { appInitialized : Result String ()
    , currentTime : Posix
    , clientId : UUID
    , searchText : String
    , stations : RemoteData Http.Error (List Station)
    , predictions : Maybe ( List Prediction, Posix )
    , selectedStation : Maybe ( Station, Maybe Station )
    }


logError : UUID -> String -> Cmd Msg
logError clientId err =
    Http.request
        { method = "POST"
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "message", E.string err )
                    , ( "clientId", E.string <| UUID.toString clientId )
                    ]
        , expect = Http.expectWhatever LoggedError
        , timeout = Just 3000
        , tracker = Nothing
        , url = "/api/log"
        , headers = []
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedPredictions jsonValue ->
            let
                result =
                    D.decodeValue (D.list Prediction.decodePrediction) jsonValue
            in
            case result of
                Ok value ->
                    ( model
                    , Task.perform (TimeStampedPredictions value) Time.now
                    )

                Err err ->
                    ( model
                    , logError model.clientId <| D.errorToString err
                    )

        TimeStampedPredictions predictions posixTime ->
            ( { model
                | predictions = Just ( predictions, posixTime )
              }
            , Cmd.none
            )

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
                , searchText = station.name
              }
            , Cmd.none
            )

        SearchTextChanged searchText ->
            ( { model
                | searchText = searchText
                , selectedStation = Nothing
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
            , logError model.clientId <| httpErrorToString httpErr
            )

        ReceivedStations (Result.Ok stations) ->
            ( { model
                | stations = Success stations
              }
            , Cmd.none
            )

        LoggedError _ ->
            -- There's nothing we can do if we have an error logging an error
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
      , predictions = Nothing
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
    let
        showStations =
            MaybeX.isJust model.selectedStation
    in
    El.layoutWith
        { options =
            [ El.focusStyle
                { borderColor = Just crimsonLight
                , backgroundColor = Just white
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
        [ Font.color white
        , El.width El.fill
        ]
        (El.column
            [ El.width El.fill
            ]
            [ El.el
                [ El.paddingXY 0 16
                , El.width El.fill
                , Border.widthEach { edges | bottom = 3 }
                , Border.color crimsonLight
                , El.htmlAttribute (attribute "role" "combobox")
                ]
                (searchInput (Maybe.map Tuple.first model.selectedStation) model.searchText)
            , El.column
                [ El.centerX
                , El.paddingXY 0 16
                , El.htmlAttribute (attribute "role" "listbox")
                , El.htmlAttribute (attribute "id" "station-results")
                , El.htmlAttribute (attribute "aria-orientation" "vertical")
                , El.htmlAttribute
                    (attribute "aria-hidden" <|
                        if showStations then
                            "false"

                        else
                            "true"
                    )
                ]
                (model.stations
                    |> RemoteData.map (Station.searchStation model.searchText)
                    |> RemoteData.map (List.map stationEl)
                    |> RemoteData.withDefault []
                    |> (\menuItems ->
                            if MaybeX.isJust model.selectedStation then
                                []

                            else
                                menuItems
                       )
                )
            , El.column
                [ El.centerX
                , El.paddingXY 0 16
                , El.htmlAttribute (attribute "aria-live" "polite")
                , El.spacingXY 0 16
                ]
                (model.selectedStation
                    |> Maybe.map (Tuple.first >> .code)
                    |> Maybe.map2
                        (\predictions code -> List.filter (.locationCode >> (==) code) predictions)
                        (Maybe.map Tuple.first model.predictions)
                    |> Maybe.map (List.map predictionEl)
                    |> Maybe.withDefault []
                )
            ]
        )


predictionEl : Prediction -> El.Element Msg
predictionEl prediction =
    El.row
        [ El.width <| El.px 320
        , Border.widthEach { edges | bottom = 2, left = 2 }
        , El.paddingEach { edges | left = 8, right = 8, bottom = 2, top = 2 }
        ]
        [ El.el
            [ El.alignLeft
            , El.width <| El.fillPortion 1
            ]
            (El.paragraph [] [ El.text prediction.destinationName ])
        , El.el
            [ El.alignRight
            , El.width <| El.fillPortion 0
            , El.paddingEach { edges | left = 8 }
            ]
            (El.text <| prediction.minutes)
        ]


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
        , El.htmlAttribute (attribute "role" "none")
        ]
        (Input.button
            [ El.width El.fill
            , Background.color crimson
            , Font.color (El.rgba255 255 255 255 1)
            , Font.semiBold
            , Border.rounded 8
            , El.paddingXY 16 16
            , El.htmlAttribute (attribute "role" "option")
            ]
            { onPress = Just <| StationSelected station
            , label = label
            }
        )


searchInput : Maybe Station -> String -> El.Element Msg
searchInput selectedStation searchText =
    Input.search
        [ Border.color crimsonLight
        , Background.color white
        , borderShadow
        , Font.color crimsonLight
        , El.focused []
        , El.width (El.px 320)
        , El.centerX
        , El.htmlAttribute (attribute "aria-controls" "station-results")
        , El.htmlAttribute (attribute "aria-autocomplete" "list")
        , El.htmlAttribute
            (attribute "aria-activedescendant"
                (selectedStation
                    |> Maybe.map .code
                    |> Maybe.withDefault ""
                )
            )
        , Event.onFocus (SearchTextChanged "")
        ]
        { label =
            Input.labelBelow
                [ El.paddingEach { edges | left = 16, top = 4 }
                , Font.color crimsonLight
                ]
                (El.text "Station Name")
        , onChange = SearchTextChanged
        , placeholder =
            Just <|
                Input.placeholder
                    [ Font.color gray
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


gray =
    El.rgba255 0 0 0 0.5


crimsonLight =
    El.rgba255 233 20 54 0.8


crimson =
    El.rgba255 233 20 54 0.8


white =
    El.rgba255 255 255 255 0.8


edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl url ->
            "Bad Url: " ++ url

        Timeout ->
            "Request Timeout"

        NetworkError ->
            "Network Error"

        BadStatus statusCode ->
            "Bad Status: " ++ String.fromInt statusCode

        BadBody decodeErr ->
            "Bad Body: " ++ decodeErr
