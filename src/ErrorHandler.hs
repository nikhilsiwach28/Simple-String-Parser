{-# LANGUAGE OverloadedStrings #-}

module ErrorHandler where

import Data.Text (Text)
import qualified Data.Text as T

data MyError = InvalidDataError | InternalServerError
  deriving (Eq, Show)


toHttpStatusCode :: MyError -> (Int, Text)
toHttpStatusCode InvalidDataError = (400, "Invalid data")
toHttpStatusCode InternalServerError = (500, "Internal server error")

