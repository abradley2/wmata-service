port module Main exposing (..)

import Array
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
import Keyboard
import Keyboard.Arrows exposing (Direction(..), arrowsDirection)
import Maybe.Extra as MaybeX
import Platform exposing (Program)
import Prediction exposing (Prediction)
import Random
import RemoteData exposing (RemoteData(..))
import Result.Extra as ResultX
import Station exposing (Station)
import SvgIcons exposing (..)
import Task
import Time exposing (Posix)
import UUID exposing (Seeds, UUID)


port blurs : (D.Value -> msg) -> Sub msg


port receivePredictions : (D.Value -> msg) -> Sub msg


port receivedLocation : (D.Value -> msg) -> Sub msg


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
    | SearchFocusToggled Bool
    | ToggleLocationConfirm Bool
    | ReceivedLocation D.Value
    | ReceivedTime Posix
    | ReceivedPredictions D.Value
    | TimeStampedPredictions (List Prediction) Posix
    | SearchTextChanged String
    | StationSelected Station
    | KeyboardMsg Int Keyboard.Msg
    | Blur D.Value
    | LoggedError (Result Http.Error ())


type alias Model =
    { appInitialized : Result String ()
    , pressedKeys : List Keyboard.Key
    , currentTime : Posix
    , locationConfirm : Bool
    , clientId : UUID
    , searchText : String
    , searchFocused : Maybe Int
    , stations : RemoteData Http.Error (List Station)
    , location : RemoteData String ( Float, Float )
    , predictions : Maybe ( List Prediction, Posix )
    , selectedStation : Maybe ( Station, Maybe Station )
    }


focusedStation : Model -> Maybe String
focusedStation model =
    case ( model.searchFocused, model.stations ) of
        ( Just idx, Success stations ) ->
            stations
                |> Station.searchStation model.searchText
                |> Array.fromList
                |> Array.get idx
                |> Maybe.map .name

        _ ->
            Nothing


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


processKeys : Model -> List Keyboard.Key -> Model
processKeys =
    List.foldl
        (\key model ->
            case key of
                Keyboard.Escape ->
                    { model | searchText = "" }

                Keyboard.Enter ->
                    { model
                        | selectedStation =
                            model.stations
                                |> RemoteData.toMaybe
                                |> Maybe.map Array.fromList
                                |> MaybeX.andThen2 Array.get model.searchFocused
                                |> Maybe.map2
                                    (\allStations station ->
                                        ( station
                                        , Station.findCoStation station allStations
                                        )
                                    )
                                    (RemoteData.toMaybe model.stations)
                    }

                _ ->
                    model
        )


checkFocusIndex : Model -> Model
checkFocusIndex model =
    case
        ( model.searchFocused
        , RemoteData.map (Station.searchStation model.searchText) model.stations
        )
    of
        ( Just idx, Success stations ) ->
            if idx < 0 then
                { model | searchFocused = Just <| List.length stations - 1 }

            else if idx >= List.length stations then
                { model | searchFocused = Just 0 }

            else
                model

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Blur _ ->
            ( { model
                | pressedKeys = []
              }
            , Cmd.none
            )

        KeyboardMsg focusedIndex keyboardMsg ->
            let
                pressedKeys =
                    Keyboard.update keyboardMsg model.pressedKeys

                nextModel =
                    { model | pressedKeys = pressedKeys }
                        |> \m -> processKeys m m.pressedKeys
            in
            case arrowsDirection nextModel.pressedKeys of
                North ->
                    ( { nextModel | searchFocused = Just <| focusedIndex - 1 } |> checkFocusIndex
                    , Cmd.none
                    )

                South ->
                    ( { nextModel | searchFocused = Just <| focusedIndex + 1 } |> checkFocusIndex
                    , Cmd.none
                    )

                _ ->
                    ( nextModel
                    , Cmd.none
                    )

        SearchFocusToggled searchFocused ->
            if searchFocused then
                ( { model
                    | searchFocused = Just 0
                    , searchText = ""
                  }
                , Cmd.none
                )

            else
                ( { model
                    | searchFocused = Nothing
                    , pressedKeys = []
                  }
                , Cmd.none
                )

        ToggleLocationConfirm locationConfirm ->
            ( { model
                | locationConfirm = locationConfirm
              }
            , Cmd.none
            )

        ReceivedLocation jsonVal ->
            let
                location =
                    D.decodeValue
                        (D.map2 Tuple.pair
                            (D.at [ "0" ] D.float)
                            (D.at [ "0" ] D.float)
                            |> remoteDataDecoder
                        )
                        jsonVal
                        |> Result.mapError (\err -> Failure (D.errorToString err))
                        |> ResultX.merge
            in
            ( { model | location = location }, Cmd.none )

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
                , searchFocused = Just 0
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

        ( clientId, _ ) =
            UUID.step flags.seeds
    in
    ( { appInitialized = flagsResult |> Result.map (always ())
      , clientId = clientId
      , locationConfirm = False
      , currentTime = Time.millisToPosix flags.now
      , location = Loading
      , searchText = ""
      , stations = Loading
      , selectedStation = Nothing
      , predictions = Nothing
      , searchFocused = Nothing
      , pressedKeys = []
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
                , backgroundColor = Just crimson
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
        , El.paddingEach { edges | top = 110 }
        , El.inFront
            (El.el
                [ El.paddingEach { edges | top = 16, bottom = 8 }
                , El.width El.fill
                , Background.color <| El.rgba255 255 255 255 1
                , Border.widthEach { edges | bottom = 4 }
                , Border.color crimsonLight
                , El.htmlAttribute (attribute "role" "combobox")
                ]
                (searchInput (focusedStation model) model.searchText)
            )
        ]
        (El.column
            [ El.width El.fill
            ]
            [ El.row
                [ El.spaceEvenly
                , El.width <| El.px 320
                , El.centerX
                , El.paddingXY 24 6
                ]
                [ El.el
                    [ El.height <| El.px 48
                    , El.width <| El.px 48
                    ]
                    (El.html starIcon)
                , Input.button
                    [ Background.color crimsonLight
                    , Border.rounded 99999
                    , El.centerX
                    ]
                    { onPress = Just <| ToggleLocationConfirm (not model.locationConfirm)
                    , label =
                        El.el
                            [ El.width <| El.px 60
                            , El.height <| El.px 60
                            ]
                            (El.html locationIcon)
                    }
                , El.el
                    [ El.height <| El.px 48
                    , El.width <| El.px 48
                    ]
                    (El.html starIcon)
                ]
            , El.column
                [ El.centerX
                , El.spacingXY 0 16
                , El.paddingEach { edges | top = 16 }
                , El.width <| El.px 320
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
                    |> RemoteData.map (List.indexedMap (stationEl model.searchFocused))
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
                , El.paddingEach { edges | top = 16 }
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
        , El.paddingXY 16 16
        , Border.width 2
        , Border.color crimson
        , Border.rounded 8
        , Font.color crimsonLight
        , Font.size 20
        , Font.semiBold
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


stationEl : Maybe Int -> Int -> Station -> El.Element Msg
stationEl focusedIdx itemIdx station =
    let
        label =
            El.paragraph [] <|
                [ El.text <|
                    station.name
                        ++ " "
                        ++ (Station.lineCodeDisplay station |> Maybe.withDefault "")
                ]

        focused =
            focusedIdx
                |> Maybe.map ((==) itemIdx)
                |> Maybe.withDefault False
    in
    Input.button
        ([ El.width El.fill
         , Font.color white
         , Font.semiBold
         , Border.rounded 8
         , El.paddingXY 16 16
         , El.htmlAttribute (attribute "role" "option")
         ]
            ++ (if focused then
                    [ Background.color crimson
                    , borderShadow
                    ]

                else
                    [ Background.color crimsonLight
                    ]
               )
        )
        { onPress = Just <| StationSelected station
        , label = label
        }


searchInput : Maybe String -> String -> El.Element Msg
searchInput activeDescendant searchText =
    Input.search
        [ Border.width 2
        , Border.color crimsonLight
        , Border.rounded 8
        , Background.color white
        , borderShadow
        , Font.color crimsonLight
        , El.focused
            [ Font.color white
            , Background.color crimson
            , Border.color crimson
            ]
        , El.width (El.px 320)
        , El.centerX
        , El.htmlAttribute (attribute "aria-controls" "station-results")
        , El.htmlAttribute (attribute "aria-autocomplete" "list")
        , El.htmlAttribute
            (attribute "aria-activedescendant"
                (activeDescendant
                    |> Maybe.withDefault ""
                )
            )
        , Event.onFocus (SearchFocusToggled True)
        , Event.onLoseFocus (SearchFocusToggled False)
        ]
        { label =
            Input.labelBelow
                [ El.paddingEach { edges | left = 16, top = 4 }
                , Font.color crimsonLight
                ]
                (El.text "Station")
        , onChange = SearchTextChanged
        , placeholder =
            Just <|
                Input.placeholder
                    [ Font.color gray
                    ]
                    (El.text "Search by Station Name")
        , text = searchText
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receivePredictions ReceivedPredictions
        , receivedLocation ReceivedLocation
        , Time.every 1000 ReceivedTime
        , case model.searchFocused of
            Just focusedIndex ->
                Sub.map (KeyboardMsg focusedIndex) Keyboard.subscriptions

            Nothing ->
                Sub.none
        , blurs Blur
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
    El.rgba255 0 0 0 0.4


crimsonLight =
    El.rgba255 233 20 54 0.75


crimson =
    El.rgba255 233 20 54 1


white =
    El.rgba255 255 255 255 1


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


remoteDataDecoder : D.Decoder a -> D.Decoder (RemoteData String a)
remoteDataDecoder valueDecoder =
    D.oneOf
        [ constDecoder "Success" (D.at [ "type" ] D.string)
            |> D.map2 (\val _ -> Success val) valueDecoder
        , constDecoder "Failure" (D.at [ "type" ] D.string)
            |> D.map2 (\err _ -> Failure err) (D.at [ "error" ] D.string)
        , constDecoder "NotAsked" (D.at [ "type" ] D.string)
            |> D.map (always NotAsked)
        , constDecoder "Loading" (D.at [ "type" ] D.string)
            |> D.map (always Loading)
        ]


constDecoder : String -> D.Decoder String -> D.Decoder String
constDecoder val decoder =
    decoder
        |> D.andThen
            (\decodedVal ->
                if decodedVal == val then
                    D.succeed val

                else
                    D.fail val
            )
