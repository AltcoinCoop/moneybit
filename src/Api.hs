{-# LANGUAGE
    OverloadedStrings
  #-}

module Api where

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS



data SupportedBases
  = Base16
  | Base64
  | Base58
  deriving (Show, Eq)

instance ToJSON SupportedBases where
  toJSON Base16 = String "base16"
  toJSON Base64 = String "base64"
  toJSON Base58 = String "base58"

instance FromJSON SupportedBases where
  parseJSON (String s) | s == "base16" = pure Base16
                       | s == "base64" = pure Base64
                       | s == "base58" = pure Base58
                       | otherwise = fail "not a prescribed string"
  parseJSON x = typeMismatch "SupportedBases" x


data TranscodeRequest = TranscodeRequest
  { transcodeFrom  :: SupportedBases
  , transcodeTo    :: SupportedBases
  , transcodeInput :: BS.ByteString
  } deriving (Show, Eq)

instance FromJSON TranscodeRequest where
  parseJSON (Object o) =
    TranscodeRequest <$> o .: "from"
                     <*> o .: "to"
                     <*> (T.encodeUtf8 <$> o .: "input")
  parseJSON x = typeMismatch "TranscodeRequest" x
