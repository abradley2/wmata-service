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
import App.Env

logSource :: LogSource
logSource = "Routes.Predictions"

predictionsRequest :: Env -> IO Request
predictionsRequest env = do
  req <- parseRequest "https://api.wmata.com/StationPrediction.svc/json/GetPrediction/All"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" (apiKey env) req

fetchPredictions_ :: Env -> LoggingT (MaybeT IO) [WMATA.Predictions.Prediction]
fetchPredictions_ env =
  LoggingT $
    \logger -> do
      let logErr = logger defaultLoc logSource LevelError
      let logInf = logger defaultLoc logSource LevelInfo

      lift $ logInf "Initializing predictionsRequest"
      req <- lift $ predictionsRequest env
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

fetchPredictions :: MonadIO m => Env -> m (Maybe [WMATA.Predictions.Prediction])
fetchPredictions env =
  liftIO . runMaybeT . runStdoutLoggingT $ fetchPredictions_ env
