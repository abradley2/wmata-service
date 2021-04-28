{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.Predictions where

import App.Env
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

predictionsRequest :: MonadIO m => Env -> m Request
predictionsRequest env =
  liftIO
    (parseRequest "https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All")
    <&> addRequestHeader "api_key" (apiKey env)
      . addRequestHeader "Accept" "application/json"

fetchPredictions_ :: Env -> LoggingT (MaybeT IO) [WMATA.Predictions.Prediction]
fetchPredictions_ env =
  LoggingT $
    \logger -> do
      let logErr = logger defaultLoc logSource LevelError
      let logInf = lift . logger defaultLoc logSource LevelInfo
      let ll msg = logLeft (logErr . (msg <>))

      logInf "Initializing predictionsRequest"
      req <- predictionsRequest env
      logInf "Start fetchPredications"
      res <-
        catchAndLogHttpException
          (logErr . ("Error fetching predictions: " <>))
          $ httpBS req
      logInf "Finished fetchPredictions"
      results <-
        ll "Error decoding api results: " $
          decodeApiResponse (getResponseBody res)
      ll "Malformed results found: " $
        sequence results
      pure $ rights results
  where
    decodeApiResponse = fmap WMATA.Predictions.results . eitherDecode . LazyB8.fromStrict

fetchPredictions :: MonadIO m => Env -> m (Maybe [WMATA.Predictions.Prediction])
fetchPredictions = liftIO . runMaybeT . runStdoutLoggingT . fetchPredictions_
