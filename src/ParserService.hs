module ParserService where

import Text.Read(readMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.List.Split (splitOn)
import Types(Value(..),ValueType(..), Pair(..))
import Utilities



parseEncodedValue :: String -> String -> Maybe Value
parseEncodedValue ('0':'0':restKey) rest    = parseDate rest
parseEncodedValue ('1':'0':restKey) rest    = parseArray (ValueType "0") rest
parseEncodedValue ('0':'1':restKey) rest    = parseNumber rest
parseEncodedValue ('1':'1':restKey) rest    = parseArray (ValueType "1") rest
parseEncodedValue ('0':'2':restKey) rest    = parseString rest
parseEncodedValue ('1':'2':restKey) rest    = parseArray (ValueType "2") rest
parseEncodedValue ('0':'3':restKey) rest    = parseBool rest
parseEncodedValue ('1':'3':restKey) rest    = parseArray (ValueType "3") rest
parseEncodedValue _ _ = Nothing

parseDate :: String -> Maybe Value
parseDate str =
  let [year, month, day] = splitOn "-" str
   in Just $ VDate (year ++ "-" ++ month ++ "-" ++ day)

parseNumber :: String -> Maybe Value
parseNumber str = VNumber <$> readMaybe str

parseString :: String -> Maybe Value
parseString str = Just $ VString str

parseBool :: String -> Maybe Value
parseBool "y" = Just $ VBool True
parseBool "Y" = Just $ VBool True
parseBool "t" = Just $ VBool True
parseBool "T" = Just $ VBool True
parseBool "n" = Just $ VBool False
parseBool "N" = Just $ VBool False
parseBool "f" = Just $ VBool False
parseBool "F" = Just $ VBool False
parseBool _ = Nothing

parseArray :: ValueType -> String -> Maybe Value
parseArray (ValueType value) str = do
  let values = splitOn "," str
  let parsedValues = catMaybes $ map (parseEncodedValue ('0' : value)) values
  Just $ VArray (getTypeForArray (ValueType value)) parsedValues

parseKeyValuePair :: String -> Maybe Pair
parseKeyValuePair str = do
  let (key, value) = break (== '|') str
  parsedValue <- parseEncodedValue key (drop 1 value)
  Just $ Pair key parsedValue


parseToJSON :: String -> Maybe (HM.HashMap T.Text Value)
parseToJSON input = do
  let pairs = splitOn "#" input
  let parsedPairs = catMaybes $ map parseKeyValuePair pairs
  let keyValuePairs = map (\(Pair key value) -> (T.pack (drop 2 key), value)) parsedPairs
  Just $ HM.fromList keyValuePairs

