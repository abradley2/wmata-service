{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server
  ( run,
  )
where

import App.DB
import App.Env
import qualified App.Routes.Predictions as Predictions
import qualified App.Routes.Stations as Stations
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import qualified Data.Text.Lazy as TextLazy
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.WebSockets
import qualified Network.Wai.Middleware.Static as Static
import Network.WebSockets
import Relude
import System.Environment
import Web.Scotty as Scotty

-- | wsApp
-- Accepts all incoming websocket connections. When
-- serving them, will retrieve the latest cached
-- rail predictions from the database and send them
-- immediately, and then once after every "socketInterval"
wsApp :: DataBase -> ServerApp
wsApp db pendingConnection =
  serveConnection db =<< liftIO (acceptRequest pendingConnection)

serveConnection :: MonadIO m => DataBase -> Connection -> m ()
serveConnection db con = do
  val <- retrieveLatestPredictions db
  liftIO $
    sendDataMessage
      con
      (Text (maybe "" LazyB8.fromStrict val) Nothing)
  liftIO socketInterval
  serveConnection db con

-- | pollPredictions
-- At an interval specified by "pollInterval", we will
-- query the WMATA API for the latest predictions and
-- store them in the Database to be distributed to the
-- websocket connections
pollPredictions :: MonadIO m => DataBase -> Env -> m ()
pollPredictions db env = do
  predictions <- Predictions.fetchPredictions env
  maybe
    (pure ())
    (storeLatestPredictions db . LazyB8.toStrict . encode)
    predictions
  liftIO pollInterval
  pollPredictions db env
  where
    encode = Data.Aeson.encode

-- | spaMiddleware
-- Simple middleware function that re-routes the url of any
-- request the accepts "text/html" to point to "index.html".
-- Enables SPA navigation for the frontend.
spaMiddleware :: Application -> Application
spaMiddleware scottyApp req = scottyApp (if isDocumentRequest then newReq else req)
  where
    requestHeaders = Network.Wai.requestHeaders
    newReq = req {pathInfo = ["index.html"]}
    isDocumentRequest =
      maybe
        False
        (B8.isInfixOf "text/html" . snd)
        (find ((==) hAccept . fst) $ requestHeaders req)

apiApp :: IO Application
apiApp = Scotty.scottyApp $ do
  middleware spaMiddleware
  middleware $ Static.staticPolicy (Static.addBase "client/dist")
  get "/api/stations" $ do
    withEnv Stations.fetchStations >>= \case
      Nothing -> do
        status status500
        text "Internal Server Error"
      Just jsonVal -> do
        status status200
        Scotty.json jsonVal
  get "/api/predictions" $ do
    withEnv Predictions.fetchPredictions >>= \case
      Nothing -> do
        status status500
        text "Internal Server Error"
      Just jsonVal -> do
        status status200
        Scotty.json jsonVal
  post "/api/log" $ do
    reqBody <- Scotty.jsonData  
    text "hi"
  where
    get = Scotty.get

app :: IO Application
app = do
  db <- openDB
  forkIO $ withEnv $ pollPredictions db
  websocketsOr defaultConnectionOptions (wsApp db) <$> apiApp

run :: IO ()
run = Warp.run 8000 =<< app

socketInterval :: IO ()
socketInterval = threadDelay $ 1000000 * 4

pollInterval :: IO ()
pollInterval = threadDelay $ 1000000 * 15
