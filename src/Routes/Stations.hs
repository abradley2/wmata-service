{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Routes.Stations where

import Control.Monad.Logger
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Simple
import Relude

logSource :: LogSource
logSource = "Routes.Stations"

stationsRequest :: B8.ByteString -> IO Request
stationsRequest apiKey = do
  req <- parseRequest "https://api.wmata.com/Rail.svc/json/jStations"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" apiKey req

fetchStations_ :: B8.ByteString -> LoggingT IO B8.ByteString 
fetchStations_ apiKey = LoggingT $ \logger -> do
  logger defaultLoc logSource LevelInfo "Creating stationsRequest"
  req <- stationsRequest apiKey
  logger defaultLoc logSource LevelInfo "Start fetchStations"
  res <- httpBS req
  logger defaultLoc logSource LevelInfo "Done stations"
  return $ getResponseBody res

fetchStations :: MonadIO m => B8.ByteString -> m B8.ByteString 
fetchStations apiKey = liftIO $ runStdoutLoggingT (fetchStations_ apiKey)
