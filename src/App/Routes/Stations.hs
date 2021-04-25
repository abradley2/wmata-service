{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.Stations where

import App.Logging
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import qualified Data.Text.Lazy as TextLazy
import qualified WMATA.Stations
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

fetchStations_ :: B8.ByteString -> LoggingT (MaybeT IO) WMATA.Stations.ApiResponse
fetchStations_ apiKey =
  LoggingT $ \logger -> do
    let logErr = logger defaultLoc logSource LevelError
    let logInf = logger defaultLoc logSource LevelInfo

    lift $ logInf "Creating stationsRequest"
    req <- lift $ stationsRequest apiKey
    lift $ logInf "Start fetchStations"
    res <-
      catchAndLogHttpException
        (logErr . (<>) "Error fecthing stations: ")
        (httpBS req)
    lift $ logger defaultLoc logSource LevelInfo "Done stations"
    logLeft
      (logErr . (<>) "Error decoding api response: ")
      (decodeApiResponse $ getResponseBody res)
  where
    decodeApiResponse :: B8.ByteString -> Either String WMATA.Stations.ApiResponse
    decodeApiResponse = eitherDecode . LazyB8.fromStrict

fetchStations :: MonadIO m => B8.ByteString -> m (Maybe WMATA.Stations.ApiResponse)
fetchStations apiKey = liftIO . runMaybeT . runStdoutLoggingT $ fetchStations_ apiKey
