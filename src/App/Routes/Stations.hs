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
import Network.HTTP.Simple
import Relude
import WMATA.Data
import qualified WMATA.Stations
import App.Env

logSource :: LogSource
logSource = "Routes.Stations"

stationsRequest :: Env -> IO Request
stationsRequest env = do
  req <- parseRequest "https://api.wmata.com/Rail.svc/json/jStations"
  return $
    addRequestHeader "Accept" "application/json" $
      addRequestHeader "api_key" (apiKey env) req

fetchStations_ :: Env -> LoggingT (MaybeT IO) [WMATA.Stations.Station]
fetchStations_ env =
  LoggingT $ \logger -> do
    let logErr = logger defaultLoc logSource LevelError
    let logInf = logger defaultLoc logSource LevelInfo

    lift $ logInf "Creating stationsRequest"
    req <- lift $ stationsRequest env
    lift $ logInf "Start fetchStations"
    res <-
      catchAndLogHttpException
        (logErr . (<>) "Error fecthing stations: ")
        (httpBS req)
    lift $ logger defaultLoc logSource LevelInfo "Done stations"
    results <-
      logLeft
        (logErr . (<>) "Error decoding api response: ")
        (decodeApiResponse $ getResponseBody res)
    logLeft
      (logErr . (<>) "Malformed results found: ")
      $ sequence results
    return $ rights results
  where
    decodeApiResponse = fmap WMATA.Stations.results . eitherDecode . LazyB8.fromStrict

fetchStations :: MonadIO m => Env -> m (Maybe [WMATA.Stations.Station])
fetchStations env = liftIO . runMaybeT . runStdoutLoggingT $ fetchStations_ env
