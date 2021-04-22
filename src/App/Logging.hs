{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.Logging where

import Control.Monad.Logger
import Relude
import Network.HTTP.Client
import Control.Exception

logLeft :: (LogStr -> IO ()) -> ExceptT String IO a -> ExceptT String IO a
logLeft logger except =
  ExceptT $
    runExceptT except
      >>= \eitherVal -> case eitherVal of
        Left err -> do
          logger $ show err
          return $ Left err
        _ ->
          return eitherVal

catchAndLogHttpException :: (LogStr -> IO ()) -> IO a -> ExceptT String IO a
catchAndLogHttpException logger getRes =
  logLeft logger $ ExceptT $ catch (Right <$> getRes) (return . handle)
  where
    handle :: HttpException -> Either String a
    handle ex = case ex of
      HttpExceptionRequest _ content -> Left $ show content
      otherEx -> throw otherEx