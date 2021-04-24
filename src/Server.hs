{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Server
  ( run
  ) where

import App.DB
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

getApiKey :: MonadIO m => m B8.ByteString
getApiKey = liftIO $ B8.pack <$> getEnv "API_KEY"

withEnv :: MonadIO m => (B8.ByteString -> m a) -> m a
withEnv action = do
  apiKey <- liftIO getApiKey
  action apiKey

-- | wsApp
-- Accepts all incoming websocket connections. When
-- serving them, will retrieve the latest cached
-- rail predictions from the database and send them
-- immediately, and then once after every "socketInterval"
wsApp :: DataBase -> ServerApp
wsApp db pendingConnection = do
  con <- liftIO $ acceptRequest pendingConnection
  serveConnection db con

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

-- | spaMiddleware
-- Simple middleware function that re-routes the url of any
-- request the accepts "text/html" to point to "index.html".
-- Enables SPA navigation for the frontend.
spaMiddleware :: Application -> Application
spaMiddleware scottyApp req respond =
  let newReq = req {pathInfo = ["index.html"]}
      isDocumentRequest =
        maybe
          False
          (B8.isInfixOf "text/html" . snd)
          (find ((==) hAccept . fst) $ requestHeaders req)
   in scottyApp
        (if isDocumentRequest
           then newReq
           else req)
        respond
  where
    requestHeaders = Network.Wai.requestHeaders

apiApp :: IO Application
apiApp =
  Scotty.scottyApp $ do
    middleware spaMiddleware
    middleware $ Static.staticPolicy (Static.addBase "client/dist")
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

app :: IO Application
app = do
  db <- openDB
  forkIO $ withEnv $ pollPredictions db
  websocketsOr defaultConnectionOptions (wsApp db) <$> apiApp

run :: IO ()
run = do
  getApiKey
  app >>= Warp.run 8000

socketInterval :: IO ()
socketInterval = do
  threadDelay $ 1000000 * 4
  return ()

pollInterval :: IO ()
pollInterval = do
  threadDelay $ 1000000 * 15
  return ()
