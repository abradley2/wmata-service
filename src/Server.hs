{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server
  ( run,
  )
where

import Control.Concurrent
import App.DB
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8 
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Middleware.Static as Static
import Network.WebSockets
import Relude
import qualified App.Routes.Predictions as Predictions
import qualified App.Routes.Stations as Stations
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
  val <- retrieveLatestPredictions db
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
  maybe
    (return ())
    (storeLatestPredictions db . LazyB8.toStrict . encode)
    predictions
  liftIO pollInterval
  pollPredictions db apiKey
  where
    encode = Data.Aeson.encode

pollInterval :: IO ()
pollInterval = do
  threadDelay $ 1000000 * 15
  return ()

apiApp :: IO Application
apiApp = Scotty.scottyApp $ do
  middleware $ Static.staticPolicy (Static.addBase "client/public") 

  get "/api/stations" $ do
    res <- withEnv Stations.fetchStations
    html $ show res

  get "/api/predictions" $ do
    res <- withEnv Predictions.fetchPredictions
    case res of
      Nothing -> do
        status status500
        text "Internal Server Error"
      Just jsonVal -> do
        status status200
        json jsonVal
  where
    json = Scotty.json
    get = Scotty.get
    decode :: B8.ByteString -> Either String Predictions.ApiResponse
    decode = eitherDecode . LazyB8.fromStrict

app :: IO Application
app = do
  db <- openDB
  forkIO $ withEnv $ pollPredictions db
  websocketsOr defaultConnectionOptions (wsApp db) <$> apiApp

run :: IO ()
run = do
  getApiKey
  app >>= Warp.run 8000