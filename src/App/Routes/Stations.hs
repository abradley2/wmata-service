{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.Stations where

import Control.Monad.Logger
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Simple
import Relude

newtype ApiResponse = ApiResponse
  { stations :: [Station]
  }

instance FromJSON ApiResponse where
  parseJSON = withObject "ApiResponse" $ \val ->
    ApiResponse <$> val .: "Stations"

instance ToJSON ApiResponse where
  toJSON val =
    object
      [ "stations" .= stations val
      ]

data Station = Station
  { code :: String,
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
      [ "code" .= code val,
        "coStation1" .= coStation1 val,
        "coStation2" .= coStation2 val,
        "lineCode1" .= lineCode1 val,
        "lineCode2" .= lineCode2 val,
        "lineCode3" .= lineCode3 val,
        "longitude" .= longitude val,
        "latitude" .= latitude val
      ]

instance FromJSON Station where
  parseJSON = withObject "Prediction" $ \val ->
    Station
      <$> val .: "Code"
      <*> val .:? "StationTogether1"
      <*> val .:? "StationTogether2"
      <*> val .: "LineCode1"
      <*> val .:? "LineCode2"
      <*> val .:? "LineCode3"
      <*> val .: "Lon"
      <*> val .: "Lat"

logSource :: LogSource
logSource = "Routes.Stations"

stationsRequest :: B8.ByteString -> IO Request
stationsRequest apiKey = do
  req <- parseRequest "https://api.wmata.com/Rail.svc/json/jStations"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" apiKey req

fetchStations_ :: B8.ByteString -> LoggingT IO B8.ByteString
fetchStations_ apiKey =
  LoggingT $ \logger -> do
    logger defaultLoc logSource LevelInfo "Creating stationsRequest"
    req <- stationsRequest apiKey
    logger defaultLoc logSource LevelInfo "Start fetchStations"
    res <- httpBS req
    logger defaultLoc logSource LevelInfo "Done stations"
    return $ getResponseBody res

fetchStations :: MonadIO m => B8.ByteString -> m B8.ByteString
fetchStations apiKey = liftIO $ runStdoutLoggingT (fetchStations_ apiKey)
