{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.Predictions where

import App.Logging
import Control.Monad.Logger
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Client
import Network.HTTP.Simple
import Relude

newtype ApiResponse = ApiResponse
  { trains :: [Prediction]
  }
  deriving (Show)

instance ToJSON ApiResponse where
  toJSON val =
    object
      [ "trains" .= trains val
      ]

instance FromJSON ApiResponse where
  parseJSON = withObject "Response" $ \val ->
    ApiResponse <$> val .: "Trains"

data Prediction = Prediction
  { car :: Maybe String,
    destination :: Maybe String,
    destinationName :: Maybe String,
    locationCode :: Maybe String,
    locationName :: Maybe String,
    minutes :: Maybe String
  }
  deriving (Show)

instance ToJSON Prediction where
  toJSON val =
    object
      [ "car" .= car val,
        "destination" .= destination val,
        "destinationName" .= destinationName val,
        "locationCode" .= locationCode val,
        "locationName" .= locationName val,
        "minutes" .= minutes val
      ]

instance FromJSON Prediction where
  parseJSON = withObject "Prediction" $ \val ->
    Prediction
      <$> val .:? "Car"
      <*> val .:? "Destination"
      <*> val .:? "DestinationName"
      <*> val .:? "LocationCode"
      <*> val .:? "LocationName"
      <*> val .:? "Min"

logSource :: LogSource
logSource = "Routes.Predictions"

predictionsRequest :: B8.ByteString -> IO Request
predictionsRequest apiKey = do
  req <- parseRequest "https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" apiKey req

fetchPredictions_ :: B8.ByteString -> LoggingT (ExceptT String IO) ApiResponse
fetchPredictions_ apiKey =
  LoggingT $
    \logger -> do
      let logErr = logger defaultLoc logSource LevelError
      let logInf = logger defaultLoc logSource LevelInfo

      lift $ logInf "Initializing predictionsRequest"
      req <- lift $ predictionsRequest apiKey
      lift $ logInf "Start fetchPredications"
      res <-
        catchAndLogHttpException
          (logErr . (<>) "Error fetching predictions: ")
          $ httpBS req
      lift $ logInf "Finished fetchPredictions"
      logLeft
        (logErr . (<>) "Error decoding api results: ")
        $ decodeApiResponse $
          getResponseBody res
  where
    decodeApiResponse :: B8.ByteString -> ExceptT String IO ApiResponse
    decodeApiResponse = hoistEither . eitherDecode . LazyB8.fromStrict

fetchPredictions :: MonadIO m => B8.ByteString -> m (Maybe ApiResponse)
fetchPredictions apiKey =
  liftIO $ fmap rightToMaybe $ runExceptT $ runStdoutLoggingT (fetchPredictions_ apiKey)