{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.Predictions where

import App.Logging
import Control.Monad.Logger
import Data.Aeson
import qualified WMATA.Predictions
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Client
import Network.HTTP.Simple
import Relude

logSource :: LogSource
logSource = "Routes.Predictions"

predictionsRequest :: B8.ByteString -> IO Request
predictionsRequest apiKey = do
  req <- parseRequest "https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" apiKey req

fetchPredictions_ :: B8.ByteString -> LoggingT (MaybeT IO) WMATA.Predictions.ApiResponse
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
        $ decodeApiResponse (getResponseBody res)
  where
    decodeApiResponse :: B8.ByteString -> Either String WMATA.Predictions.ApiResponse
    decodeApiResponse = eitherDecode . LazyB8.fromStrict

fetchPredictions :: MonadIO m => B8.ByteString -> m (Maybe WMATA.Predictions.ApiResponse)
fetchPredictions apiKey =
  liftIO . runMaybeT . runStdoutLoggingT $ fetchPredictions_ apiKey