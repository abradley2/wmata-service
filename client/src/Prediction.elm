module Prediction exposing (..)

import Json.Decode as D


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
