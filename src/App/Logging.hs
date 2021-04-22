{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module App.Logging where

import Control.Monad.Logger
import Relude
import Network.HTTP.Client
import Control.Exception

logLeft logger = logIOLeft logger . return 

logIOLeft :: (LogStr -> IO ()) -> IO (Either String a)-> MaybeT IO a
logIOLeft logger ioEither =
  MaybeT $
    ioEither
      >>= \case
        Left err -> do
          logger $ show err
          return Nothing
        Right val ->
          return (Just val)

catchAndLogHttpException :: (LogStr -> IO ()) -> IO a -> MaybeT IO a
catchAndLogHttpException logger getRes =
  logIOLeft logger $ catch (Right <$> getRes) (return . handle)
  where
    handle :: HttpException -> Either String a
    handle ex = case ex of
      HttpExceptionRequest _ content -> Left $ show content
      otherEx -> throw otherEx