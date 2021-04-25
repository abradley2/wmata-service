{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module WMATA.Predictions where

import WMATA.Data

import Data.Aeson
import Data.Aeson.Types
import Relude

newtype ApiResponse = ApiResponse
  { trains :: [ParsedPrediction]
  }
  deriving (Show)

newtype ParsedPrediction = ParsedPrediction {parsedPrediction :: Result Prediction}
  deriving (Show)

results = fmap resultToEither . fmap parsedPrediction . trains

instance ToJSON ApiResponse where
  toJSON val =
    object
      [ "trains" .= trains val
      ]

instance FromJSON ApiResponse where
  parseJSON = withObject "Response" $ \val ->
    ApiResponse <$> val .: "Trains"

data Prediction = Prediction
  { line :: String,
    destination :: String,
    destinationName :: String,
    locationCode :: String,
    locationName :: String,
    minutes :: String
  }
  deriving (Show)

instance ToJSON Prediction where
  toJSON val =
    object
      [ "line" .= line val,
        "destination" .= destination val,
        "destinationName" .= destinationName val,
        "locationCode" .= locationCode val,
        "locationName" .= locationName val,
        "minutes" .= minutes val
      ]

instance ToJSON ParsedPrediction where
  toJSON =
    \case
      Success a -> toJSON a
      Error err -> object ["error" .= err]
      . parsedPrediction

instance FromJSON Prediction where
  parseJSON = withObject "Prediction" $ \val ->
    Prediction
      <$> val .: "Line"
      <*> val .: "Destination"
      <*> val .: "DestinationName"
      <*> val .: "LocationCode"
      <*> val .: "LocationName"
      <*> val .: "Min"

instance FromJSON ParsedPrediction where
  parseJSON = return . ParsedPrediction . enhancedParse parseJSON
