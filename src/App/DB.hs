{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.DB where

import Data.Aeson (ToJSON (toJSON), Value, object, encode)
import qualified Data.ByteString.Lazy.Char8 as LazyB8
import Data.Time.Clock.POSIX
import Database.RocksDB.Base
import Relude

type DataBase = DB

cachedPredictionsKey = "__predictions__"

openDB :: MonadIO m => m DB
openDB = open "./__db__" $ defaultOptions {createIfMissing = True}

retrieveLatestPredictions :: MonadIO m => DB -> m (Maybe ByteString)
retrieveLatestPredictions db =
  get db defaultReadOptions cachedPredictionsKey
  where
    get = Database.RocksDB.Base.get

storeLatestPredictions :: (ToJSON a, MonadIO m) => DB -> TimeStamped a -> m ()
storeLatestPredictions db =
  put db defaultWriteOptions cachedPredictionsKey . LazyB8.toStrict . encode . toJSON
  where
    put = Database.RocksDB.Base.put

data TimeStamped a = TimeStamped
  { time :: POSIXTime,
    value :: a
  }

instance ToJSON a => ToJSON (TimeStamped a) where
  toJSON ts =
    object
      [ ("value", toJSON $ value ts)
      , ("timestamp", toJSON $ time ts )
      ]