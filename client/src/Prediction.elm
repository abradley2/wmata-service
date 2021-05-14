module Prediction exposing (..)

import Json.Decode as D
import Json.Encode as E


type alias Prediction =
    { line : String
    , destination : String
    , destinationName : String
    , locationCode : String
    , locationName : String
    , minutes : String
    }


decodePrediction : D.Decoder Prediction
decodePrediction =
    D.map6
        Prediction
        (D.at [ "line" ] D.string)
        (D.at [ "destination" ] D.string)
        (D.at [ "destinationName" ] D.string)
        (D.at [ "locationCode" ] D.string)
        (D.at [ "locationName" ] D.string)
        (D.at [ "minutes" ] D.string)


encodePrediction : Prediction -> D.Value
encodePrediction prediction =
    E.object
        [ ( "line", E.string prediction.line )
        , ( "destination", E.string prediction.destination )
        , ( "destinationName", E.string prediction.destinationName )
        , ( "locationCode", E.string prediction.locationCode )
        , ( "locationName", E.string prediction.locationName )
        , ( "minutes", E.string prediction.minutes )
        ]
