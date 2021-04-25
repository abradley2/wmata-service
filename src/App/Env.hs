{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Env where

import Relude
import qualified Data.ByteString.Char8 as B8
import System.Environment

newtype Env = Env { apiKey :: B8.ByteString }

getApiKey :: MonadIO m => m B8.ByteString
getApiKey = liftIO $ B8.pack <$> getEnv "API_KEY"

withEnv :: MonadIO m => (Env -> m a) -> m a
withEnv action = do
  env <- Env <$> getApiKey
  action env