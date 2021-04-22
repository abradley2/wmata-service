{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module App.DB where

import Database.RocksDB.Base
import Relude

type DataBase = DB

dbName = "__predictions__"

openDB :: MonadIO m => m DB
openDB = open "./__db__" $ defaultOptions {createIfMissing = True}

retrieveLatestPredictions :: MonadIO m => DB -> m (Maybe ByteString)
retrieveLatestPredictions db =
  get db defaultReadOptions dbName
  where
    get = Database.RocksDB.Base.get

storeLatestPredictions :: MonadIO m => DB -> ByteString -> m ()
storeLatestPredictions db =
  put db defaultWriteOptions dbName
  where
    put = Database.RocksDB.Base.put
