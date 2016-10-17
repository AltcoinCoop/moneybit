{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , FlexibleInstances
  #-}

module Api.WebSocket where

import Api.WebSocket.RPC
import Api

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Control.Applicative


instance FromJSON (WSRPC NewRequest) where
  parseJSON (Object o) = do
    v <- o .: "version"
    m <- o .: "method"
    if v /= version
    then fail $ "Version string not equal to " ++ show version
    else if m /= "new"
    then fail $ "Method string not equal to \"new\""
    else do
      iv <- o .:? "interval" .!= 1000 -- milliseconds
      ps <- o .:  "params"
      i  <- o .:  "id"
      c  <- o .:? "cancel" .!= False
      pure WSRPC
        { wsMethod   = m
        , wsParams   = ps
        , wsIdent    = i
        , wsInterval = iv
        , wsComplete = False
        , wsCancel   = c
        }
  parseJSON x = typeMismatch "WSRPC" x


instance FromJSON (WSRPC OpenRequest) where
  parseJSON (Object o) = do
    v <- o .: "version"
    m <- o .: "method"
    if v /= version
    then fail $ "Version string not equal to " ++ show version
    else if m /= "open"
    then fail $ "Method string not equal to \"open\""
    else do
      iv <- o .:? "interval" .!= 1000 -- milliseconds
      ps <- o .:  "params"
      i  <- o .:  "id"
      c  <- o .:? "cancel" .!= False
      pure WSRPC
        { wsMethod   = m
        , wsParams   = ps
        , wsIdent    = i
        , wsInterval = iv
        , wsComplete = False
        , wsCancel   = c
        }
  parseJSON x = typeMismatch "WSRPC" x




data WSSubscribe
  = WSSubNew (WSRPC NewRequest)
  | WSSubOpen (WSRPC OpenRequest)


data WSSupply

data WSReply

data WSComplete

instance FromJSON WSSubscribe where
  parseJSON x = (WSSubNew  <$> parseJSON x)
            <|> (WSSubOpen <$> parseJSON x)
