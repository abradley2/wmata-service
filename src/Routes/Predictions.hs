{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Routes.Predictions where

import Control.Monad.Logger
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as TextLazy
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

fetchPredictions_ :: B8.ByteString -> LoggingT IO B8.ByteString
fetchPredictions_ apiKey =
  LoggingT $
    \logger -> do
      logger defaultLoc logSource LevelInfo "Initializing predictionsRequest"
      req <- predictionsRequest apiKey
      logger defaultLoc logSource LevelInfo "Start fetchPredications"
      res <- httpBS req
      logger defaultLoc logSource LevelInfo "Finished fetchPredictions"
      return $ getResponseBody res

fetchPredictions :: MonadIO m => B8.ByteString -> m B8.ByteString
fetchPredictions apiKey =
  liftIO $ runStdoutLoggingT (fetchPredictions_ apiKey)