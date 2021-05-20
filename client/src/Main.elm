port module Main exposing (..)

import Array
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Browser.Dom as Dom
import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font
import Element.Input as Input
import GeoJson exposing (Position)
import Html as H
import Html.Attributes exposing (attribute)
import Http exposing (Error(..))
import Json.Decode as D
import Json.Encode as E
import Keyboard
import Keyboard.Arrows exposing (Direction(..), arrowsDirection)
import Maybe.Extra as MaybeX exposing (isJust)
import Platform exposing (Program)
import Prediction exposing (Prediction)
import Random
import RemoteData exposing (RemoteData(..))
import Result.Extra as ResultX
import Station exposing (Station)
import SvgIcons exposing (..)
import Task
import Time exposing (Posix)
import Tuple3
import Turf
import UUID exposing (Seeds, UUID)


port blurs : (D.Value -> msg) -> Sub msg


port receivePredictions : (D.Value -> msg) -> Sub msg


port receivedLocation : (D.Value -> msg) -> Sub msg


port askPosition : () -> Cmd msg


type Effect
    = NoEffect
    | BatchEffect (List Effect)
    | AskPositionEffect
    | LogErrorEffect UUID String
    | BlurInputEffect String
    | FetchStationsEffect


effectToCmd : Effect -> Cmd Msg
effectToCmd eff =
    case eff of
        NoEffect ->
            Cmd.none

        BatchEffect effs ->
            Cmd.batch <| List.map effectToCmd effs

        AskPositionEffect ->
            askPosition ()

        BlurInputEffect inputId ->
            Task.attempt InputBlurred (Dom.blur inputId)

        LogErrorEffect clientId errMsg ->
            logError clientId errMsg

        FetchStationsEffect ->
            Http.request
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
    | AskPosition
    | SearchFocusToggled Bool
    | ToggleLocationConfirm Bool
    | ReceivedLocation D.Value
    | ReceivedTime Posix
    | ReceivedPredictions D.Value
    | TimeStampedPredictions (List Prediction) Posix
    | SearchTextChanged String
    | StationSelected Station
    | KeyboardMsg Keyboard.Msg
    | Blur D.Value
    | InputBlurred (Result Dom.Error ())
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
    , location : RemoteData String Position
    , predictions : Maybe ( List Prediction, Posix )
    , selectedStation : Maybe ( Station, Maybe Station )
    , nearbyStations : Maybe (List Station)
    }


displayStations : Model -> List Station
displayStations model =
    case model.searchText of
        "" ->
            model.nearbyStations
                |> Maybe.withDefault []

        _ ->
            model.stations
                |> RemoteData.toMaybe
                |> Maybe.map (Station.searchStation model.searchText)
                |> Maybe.withDefault []


focusedStation : Model -> Maybe String
focusedStation model =
    displayStations model
        |> Array.fromList
        |> Just
        |> MaybeX.andThen2 Array.get model.searchFocused
        |> Maybe.map .code


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


processKeys : Model -> ( Model, Effect )
processKeys m =
    List.foldl
        (\key ( model, eff ) ->
            case key of
                Keyboard.ArrowUp ->
                    ( { model | searchFocused = Maybe.map (flip (-) 1) model.searchFocused } |> checkFocusIndex
                    , NoEffect
                    )

                Keyboard.ArrowDown ->
                    ( { model | searchFocused = Maybe.map ((+) 1) model.searchFocused } |> checkFocusIndex
                    , NoEffect
                    )

                Keyboard.Escape ->
                    ( { model | searchText = "" }, eff )

                Keyboard.Enter ->
                    { model
                        | selectedStation =
                            MaybeX.andThen2
                                Array.get
                                model.searchFocused
                                (displayStations model |> Array.fromList |> Just)
                                |> Maybe.map2
                                    (\allStations station ->
                                        ( station
                                        , Station.findCoStation station allStations
                                        )
                                    )
                                    (RemoteData.toMaybe model.stations)
                    }
                        |> (\nextModel ->
                                ( { nextModel
                                    | searchText =
                                        nextModel.selectedStation
                                            |> Maybe.map (Tuple.first >> .name)
                                            |> Maybe.withDefault nextModel.searchText
                                  }
                                , if MaybeX.isJust nextModel.selectedStation then
                                    BatchEffect [ eff, BlurInputEffect "search-input" ]

                                  else
                                    eff
                                )
                           )

                _ ->
                    ( model, eff )
        )
        ( m, NoEffect )
        m.pressedKeys


checkFocusIndex : Model -> Model
checkFocusIndex model =
    case
        ( model.searchFocused
        , displayStations model
        )
    of
        ( Just idx, stations ) ->
            if idx < 0 then
                { model | searchFocused = Just <| List.length stations - 1 }

            else if idx >= List.length stations then
                { model | searchFocused = Just 0 }

            else
                model

        _ ->
            model


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        AskPosition ->
            ( { model | locationConfirm = False }
            , AskPositionEffect
            )

        InputBlurred _ ->
            ( model, NoEffect )

        Blur _ ->
            ( { model
                | pressedKeys = []
              }
            , NoEffect
            )

        KeyboardMsg keyboardMsg ->
            { model
                | pressedKeys =
                    Keyboard.update
                        keyboardMsg
                        model.pressedKeys
            }
                |> processKeys

        SearchFocusToggled searchFocused ->
            if searchFocused then
                ( { model
                    | searchFocused = Just 0
                    , searchText = ""
                    , selectedStation = Nothing
                  }
                , NoEffect
                )

            else
                ( { model
                    | searchFocused = Nothing
                    , pressedKeys = []
                  }
                , NoEffect
                )

        ToggleLocationConfirm locationConfirm ->
            ( { model
                | locationConfirm = locationConfirm
              }
            , NoEffect
            )

        ReceivedLocation jsonVal ->
            let
                location =
                    D.decodeValue
                        (D.map3 Tuple3.join
                            (D.at [ "0" ] D.float)
                            (D.at [ "1" ] D.float)
                            (D.succeed 0)
                            |> remoteDataDecoder
                        )
                        jsonVal
                        |> Result.mapError (\err -> Failure (D.errorToString err))
                        |> ResultX.merge
            in
            ( { model
                | location = location
                , locationConfirm =
                    -- open this dialog if there's an error to inform the user
                    case location of
                        Failure _ ->
                            True

                        _ ->
                            False
              }
                |> updateNearbyStations
            , case location of
                Failure errString ->
                    LogErrorEffect model.clientId errString

                _ ->
                    NoEffect
            )

        ReceivedPredictions jsonValue ->
            let
                result =
                    D.decodeValue
                        (D.map2
                            Tuple.pair
                            (D.field "value" <| D.list Prediction.decodePrediction)
                            (D.field "timestamp" <| D.float)
                        )
                        jsonValue
            in
            case result of
                Ok ( value, timeStamp ) ->
                    ( { model
                        | predictions =
                            Just
                                ( value
                                , Time.millisToPosix <| (floor timeStamp * 1000)
                                )
                      }
                    , NoEffect
                    )

                Err err ->
                    ( model
                    , LogErrorEffect model.clientId <| D.errorToString err
                    )

        TimeStampedPredictions predictions posixTime ->
            ( { model
                | predictions = Just ( predictions, posixTime )
                , currentTime = posixTime
              }
            , NoEffect
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
            , NoEffect
            )

        SearchTextChanged searchText ->
            ( { model
                | searchText = searchText
                , selectedStation = Nothing
                , searchFocused = Just 0
              }
            , NoEffect
            )

        ReceivedTime time ->
            ( { model
                | currentTime = time
              }
            , NoEffect
            )

        ReceivedStations (Result.Err httpErr) ->
            ( { model
                | stations = Failure httpErr
              }
                |> updateNearbyStations
            , LogErrorEffect model.clientId <| httpErrorToString httpErr
            )

        ReceivedStations (Result.Ok stations) ->
            ( { model
                | stations = Success stations
              }
            , NoEffect
            )

        LoggedError _ ->
            -- There's nothing we can do if we have an error logging an error
            ( model, NoEffect )


updateNearbyStations : Model -> Model
updateNearbyStations model =
    case ( model.stations, model.location ) of
        ( Success stations, Success location ) ->
            { model
                | nearbyStations =
                    List.take 3
                        >> Just
                    <|
                        List.sortBy (.latLng >> Turf.getDistance location) stations
            }

        _ ->
            model


init : D.Value -> ( Model, Effect )
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
      , nearbyStations = Nothing
      }
    , FetchStationsEffect
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
                (searchInput (MaybeX.isJust model.searchFocused) (focusedStation model) model.searchText)
            )
        ]
        (El.column
            [ El.width El.fill
            ]
            [ case model.predictions of
                Just ( _, timestamp ) ->
                    let
                        elapsed =
                            String.fromInt <|
                                (Time.posixToMillis model.currentTime - Time.posixToMillis timestamp)
                                    // 1000
                    in
                    El.el
                        [ El.width El.fill
                        , Font.size 14
                        , Font.center
                        , Font.color gray
                        ]
                        (El.text <| "WMATA data is " ++ elapsed ++ " seconds old")

                _ ->
                    El.none
            , El.row
                [ El.spaceEvenly
                , El.width <| El.px 320
                , El.centerX
                , El.paddingXY 24 6
                , El.inFront
                    (if model.locationConfirm then
                        El.el
                            [ El.width El.fill
                            ]
                            (locationConfirmEl model)

                     else
                        El.none
                    )
                , El.htmlAttribute (attribute "aria-live" "polite")
                ]
                [ El.el
                    [ El.height <| El.px 48
                    , El.width <| El.px 48
                    ]
                    (El.html starIcon)
                , case model.location of
                    Success _ ->
                        El.el
                            [ El.height <| El.px 48
                            , El.width <| El.px 48
                            ]
                            (El.html starIcon)

                    _ ->
                        Input.button
                            [ Background.color crimsonLight
                            , Border.rounded 99999
                            , El.centerX
                            , El.htmlAttribute (attribute "tabindex" "2")
                            , El.htmlAttribute (attribute "aria-haspopup" "true")
                            , El.htmlAttribute (attribute "data-test" "location-button")
                            ]
                            { onPress = Just <| ToggleLocationConfirm (not model.locationConfirm)
                            , label =
                                El.el
                                    [ El.width <| El.px 60
                                    , El.height <| El.px 60
                                    , El.padding 4
                                    ]
                                    (El.html <|
                                        case model.location of
                                            Failure _ ->
                                                gpsOff

                                            _ ->
                                                gpsUnknown
                                    )
                            }
                , El.el
                    [ El.height <| El.px 48
                    , El.width <| El.px 48
                    ]
                    (El.html starIcon)
                ]
            , El.column
                [ El.htmlAttribute (attribute "aria-live" "polite")
                , El.width <| El.px 320
                , El.centerX
                , El.spacingXY 0 12
                , Font.color crimsonLight
                ]
                (case model.selectedStation of
                    Just ( station, Just coStation ) ->
                        [ stationEl model.searchFocused False -100 coStation
                        , El.el [] (El.text <| "Showing " ++ Station.lineCodeDisplay station ++ " line times")
                        ]

                    _ ->
                        [ El.none ]
                )
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
                (case model.searchText of
                    "" ->
                        model.nearbyStations
                            |> Maybe.withDefault []
                            |> List.indexedMap (stationEl model.searchFocused True)

                    _ ->
                        model.stations
                            |> RemoteData.map (Station.searchStation model.searchText)
                            |> RemoteData.map (List.indexedMap (stationEl model.searchFocused False))
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


locationConfirmEl : Model -> El.Element Msg
locationConfirmEl model =
    El.column
        [ El.paddingXY 24 24
        , El.centerX
        , El.htmlAttribute (attribute "style" "z-index: 10;")
        , El.htmlAttribute (attribute "data-test" "location-confirm-popup")
        , borderShadow
        , Background.color white
        , Border.rounded 8
        , Font.color white
        ]
        [ El.el
            [ Font.color gray
            ]
            (case model.location of
                Failure _ ->
                    El.paragraph
                        []
                        [ El.text "Failed to load your location. You may need to reset permissions for this site, then try again."
                        ]

                _ ->
                    El.paragraph
                        []
                        [ El.text "Your browser will request that this site be given location permissions to automatically find the station nearest you."
                        ]
            )
        , El.row
            [ El.spacingXY 24 0
            , El.paddingEach { edges | top = 16 }
            ]
            [ Input.button
                [ El.paddingXY 16 8
                , El.htmlAttribute (attribute "tabindex" "2")
                , Border.rounded 8
                , Background.color crimsonLight
                ]
                { label = El.text "Cancel"
                , onPress = Just <| ToggleLocationConfirm False
                }
            , Input.button
                [ El.paddingXY 16 8
                , El.htmlAttribute (attribute "tabindex" "2")
                , Border.rounded 8
                , Background.color crimsonLight
                ]
                { label = El.text "Allow Location"
                , onPress = Just <| AskPosition
                }
            ]
        ]


predictionEl : Prediction -> El.Element Msg
predictionEl prediction =
    El.row
        [ El.width <| El.px 320
        , El.paddingXY 16 16
        , El.htmlAttribute (attribute "data-test" "prediction-el")
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


stationEl : Maybe Int -> Bool -> Int -> Station -> El.Element Msg
stationEl focusedIdx isNearby itemIdx station =
    let
        label =
            El.paragraph [] <|
                [ El.text <|
                    station.name
                        ++ " "
                        ++ (if isJust station.coStations.coStation1 then
                                Station.lineCodeDisplay station

                            else
                                ""
                           )
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
         , El.onLeft <|
            if isNearby then
                El.el
                    [ El.width <| El.px 28
                    , El.height <| El.px 28
                    , El.paddingEach { edges | right = 8 }
                    , El.moveRight 36
                    , El.moveDown 14
                    ]
                    (El.html gpsOn)

            else
                El.none
         , El.paddingEach
            { top = 16
            , bottom = 16
            , left =
                if isNearby then
                    36

                else
                    16
            , right = 16
            }
         , El.htmlAttribute (attribute "tabindex" "3")
         , El.htmlAttribute (attribute "role" "option")
         , El.htmlAttribute (attribute "data-test-option-idx" (String.fromInt itemIdx))
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


searchInput : Bool -> Maybe String -> String -> El.Element Msg
searchInput focused activeDescendant searchText =
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
        , El.htmlAttribute (attribute "tabindex" "1")
        , El.htmlAttribute (attribute "id" "search-input")
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
        , Time.every 150 ReceivedTime
        , case model.searchFocused of
            Just focusedIndex ->
                Sub.map KeyboardMsg Keyboard.subscriptions

            Nothing ->
                Sub.none
        , blurs Blur
        ]


main : Program D.Value Model Msg
main =
    element
        { update = \msg model -> update msg model |> Tuple.mapSecond effectToCmd
        , init = \flagsJson -> init flagsJson |> Tuple.mapSecond effectToCmd
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
            |> D.map2 (\val _ -> Success val) (D.at [ "value" ] valueDecoder)
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
