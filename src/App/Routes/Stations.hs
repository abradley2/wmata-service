{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.Stations where

import App.Env
import App.Logging
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Simple
import Relude
import WMATA.Data
import qualified WMATA.Stations

logSource :: LogSource
logSource = "Routes.Stations"

stationsRequest :: MonadIO m => Env -> m Request
stationsRequest env =
  liftIO
    (parseRequest "https://api.wmata.com/Rail.svc/json/jStations")
    <&> addRequestHeader "api_key" (apiKey env)
      . addRequestHeader "Accept" "application/json"

fetchStations_ :: Env -> LoggingT (MaybeT IO) [WMATA.Stations.Station]
fetchStations_ env =
  LoggingT $ \logger -> do
    let logErr = logger defaultLoc logSource LevelError
    let logInf = lift . logger defaultLoc logSource LevelInfo
    let ll msg = logLeft (logErr . (msg <>))

    logInf "Creating stationsRequest"
    req <- stationsRequest env
    logInf "Start fetchStations"
    res <-
      catchAndLogHttpException
        (logErr . ("Error fecthing stations: " <>))
        (httpBS req)
    logInf "Done stations"
    results <-
      ll "Error decoding api response: " $ decodeApiResponse (getResponseBody res)
    ll "Malformed results found: " $ sequence results
    return $ rights results
  where
    decodeApiResponse = fmap WMATA.Stations.results . eitherDecode . LazyB8.fromStrict

fetchStations :: MonadIO m => Env -> m (Maybe [WMATA.Stations.Station])
fetchStations = liftIO . runMaybeT . runStdoutLoggingT . fetchStations_
