{-# LANGUAGE OverloadedStrings #-}

module Types where



import Data.Aeson(encode, toJSON, ToJSON, object, (.=))
import Data.Text (Text)

data ValueType = ValueType String deriving Show  -- Element Type inside Array 

data Value  -- Custom Type for my Value 
  = VDate String
  | VNumber Int
  | VString String
  | VBool Bool
  | VArray String [Value]
  deriving Show

instance ToJSON Value where
  toJSON (VDate str)      = object ["type" .= ("date" :: Text), "value" .= str]
  toJSON (VNumber num)    = object ["type" .= ("number" :: Text), "value" .= num]
  toJSON (VString str)    = object ["type" .= ("string" :: Text), "value" .= str]
  toJSON (VBool bool)     = object ["type" .= ("bool" :: Text), "value" .= bool]
  toJSON (VArray vt vals) = object ["type" .= show vt, "values" .= vals]

data Pair = Pair String Value deriving Show