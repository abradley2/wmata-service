{-# LANGUAGE LambdaCase #-}
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
import WMATA.Data
import qualified WMATA.Predictions

logSource :: LogSource
logSource = "Routes.Predictions"

predictionsRequest :: B8.ByteString -> IO Request
predictionsRequest apiKey = do
  req <- parseRequest "https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" apiKey req

fetchPredictions_ :: B8.ByteString -> LoggingT (MaybeT IO) [WMATA.Predictions.Prediction]
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
      results <-
        logLeft
          (logErr . (<>) "Error decoding api results: ")
          $ decodeApiResponse (getResponseBody res)
      logLeft
        (logErr . (<>) "Malformed results found: ")
        $ sequence results
      return $ rights results
  where
    decodeApiResponse = fmap WMATA.Predictions.results . eitherDecode . LazyB8.fromStrict

fetchPredictions :: MonadIO m => B8.ByteString -> m (Maybe [WMATA.Predictions.Prediction])
fetchPredictions apiKey =
  liftIO . runMaybeT . runStdoutLoggingT $ fetchPredictions_ apiKey
