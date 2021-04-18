{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server
  ( run,
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import DB
import qualified Data.ByteString.Char8 as B8
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Relude
import qualified Routes.Predictions as Predictions
import qualified Routes.Stations as Stations
import System.Environment
import Web.Scotty as Scotty

getApiKey :: MonadIO m => m B8.ByteString
getApiKey =
  liftIO $ B8.pack <$> getEnv "API_KEY"

withEnv :: MonadIO m => (B8.ByteString -> m a) -> m a
withEnv action = do
  apiKey <- liftIO getApiKey
  action apiKey

wsApp :: DataBase -> ServerApp
wsApp db pendingConnection = do
  con <- liftIO $ acceptRequest pendingConnection
  serveConnection db con

serveConnection :: MonadIO m => DataBase -> Connection -> m ()
serveConnection db con = do
  val <- DB.retrieveLatestPredictions db
  liftIO $ sendDataMessage con $ Text (show val) Nothing
  liftIO socketInterval
  serveConnection db con

socketInterval :: IO ()
socketInterval = do
  threadDelay $ 1000000 * 4
  return ()

pollPredictions :: MonadIO m => DataBase -> B8.ByteString -> m ()
pollPredictions db apiKey = do
  predictions <- Predictions.fetchPredictions apiKey
  DB.storeLatestPredictions db predictions
  liftIO pollInterval
  pollPredictions db apiKey

pollInterval :: IO ()
pollInterval = do
  threadDelay $ 1000000 * 15
  return ()

apiApp :: IO Application
apiApp = Scotty.scottyApp $ do
  get "/api/stations" $ do
    res <- withEnv Stations.fetchStations
    html $ show res

  get "/api/predictions" $ do
    res <- withEnv Predictions.fetchPredictions
    html $ show res
  where
    get = Scotty.get

app :: IO Application
app = do
  db <- DB.openDB
  forkIO $ withEnv $ pollPredictions db
  websocketsOr defaultConnectionOptions (wsApp db) <$> apiApp

run :: IO ()
run = do
  getApiKey
  app >>= Warp.run 8000