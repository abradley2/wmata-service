{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WMATA.Stations where

import Data.Aeson
import Data.Aeson.Types
import Relude

newtype ApiResponse = ApiResponse
  { stations :: [ParsedStation]
  }

newtype ParsedStation = ParsedStation {parsedStation :: Result Station}

instance FromJSON ApiResponse where
  parseJSON = withObject "ApiResponse" $ \val ->
    ApiResponse <$> (val .: "Stations")

instance ToJSON ApiResponse where
  toJSON val =
    object
      [ "stations" .= stations val
      ]

data Station = Station
  { name :: String,
    code :: String,
    coStation1 :: Maybe String,
    coStation2 :: Maybe String,
    lineCode1 :: String,
    lineCode2 :: Maybe String,
    lineCode3 :: Maybe String,
    longitude :: Float,
    latitude :: Float
  }

instance ToJSON Station where
  toJSON val =
    object
      [ "name" .= name val,
        "code" .= code val,
        "coStation1" .= coStation1 val,
        "coStation2" .= coStation2 val,
        "lineCode1" .= lineCode1 val,
        "lineCode2" .= lineCode2 val,
        "lineCode3" .= lineCode3 val,
        "longitude" .= longitude val,
        "latitude" .= latitude val
      ]

stationParser :: Object -> Parser Station
stationParser val =
  Station
    <$> val .: "Name"
    <*> val .: "Code"
    <*> val .:? "StationTogether1"
    <*> val .:? "StationTogether2"
    <*> val .: "LineCode1"
    <*> val .:? "LineCode2"
    <*> val .:? "LineCode3"
    <*> val .: "Lon"
    <*> val .: "Lat"

instance ToJSON ParsedStation where
  toJSON =
    \case
      Success a -> toJSON a
      Error err -> object ["error" .= err]
      . parsedStation

instance FromJSON ParsedStation where
  parseJSON =
    return . ParsedStation . parse (withObject "Prediction" stationParser)
