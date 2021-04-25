module WMATA.Data where

import Data.Aeson
import Data.Aeson.Types

resultToEither :: Result a -> Either String a
resultToEither (Success a) = Right a
resultToEither (Error err) = Left err
