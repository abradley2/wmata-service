{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Routes.ClientLogging where

import App.Env (Env)
import App.Logging
import Control.Monad.Logger as Logger
import Data.Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import Network.Wai
import Relude

data LogErrorBody = LogErrorBody {message :: String, clientId :: String} deriving (Show)

instance FromJSON LogErrorBody where
  parseJSON =
    withObject "LogErrorBody" $ \val ->
      LogErrorBody
        <$> val .: "message"
        <*> val .: "clientId"

toValue :: B8.ByteString -> Either String Value
toValue = eitherDecode . LazyB8.fromStrict

logSource = "ClientLogging.hs"

logClientError_ :: B8.ByteString -> LoggingT (MaybeT IO) ()
logClientError_ reqBody =
  LoggingT $ \logger -> do
    let logError str = logger defaultLoc logSource LevelError . (str <>)

    logPayload <-
      logLeft
        (logError "Error decoding request payload: ")
        (parseJSON <$> toValue reqBody)
    liftIO $ logger defaultLoc "ClientLogging.hs" LevelError "reqBody"
  where
    parseJSON :: Value -> Either String LogErrorBody
    parseJSON =
      ( \case
          Error err -> Left err
          Success a -> Right a
      )
        . Data.Aeson.fromJSON

logClientError :: MonadIO m => B8.ByteString -> m (Maybe ())
logClientError =
  liftIO . runMaybeT . runStdoutLoggingT . logClientError_