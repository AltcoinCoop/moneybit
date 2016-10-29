{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , FlexibleInstances
  #-}

module Api.WebSocket.RPC where

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T


version :: T.Text
version = "0.0.1"



data WSRPC a = WSRPC
  { wsMethod   :: {-# UNPACK #-} !T.Text
  , wsParams   :: a
  , wsIdent    :: {-# UNPACK #-} !T.Text
  , wsInterval :: {-# UNPACK #-} !Int
    -- ^ In microseconds
  , wsComplete :: {-# UNPACK #-} !Bool
    -- ^ Only holds meaning for server responses and client expectations
  , wsCancel   :: {-# UNPACK #-} !Bool
    -- ^ Only holds meaning for client supplies and server processing
  }
  -- FIXME Complete should only apply to OutgoingCommand, while cancel
  --   only applies to IncomingCommand



--instance ToJSON a => ToJSON (Response WSRPC a) where
--  toJSON (Response WSRPC{..}) = object
--    [ "params"   .= wsParams
--    , "version"  .= version
--    , "id"       .= wsIdent
--    , "complete" .= wsComplete
--    ]
--
--instance FromJSON a => FromJSON (Request WSRPC a) where
--  parseJSON (Object o) = do
--    v <- o .: "version"
--    if v /= version
--    then fail $ "Version string not equal to " ++ show version
--    else do
--      m  <- o .:? "method"   .!= ""
--      iv <- o .:? "interval" .!= 1000 -- milliseconds
--      ps <- o .:  "params"
--      i  <- o .:  "id"
--      c  <- o .:? "cancel" .!= False
--      pure $ Request WSRPC
--        { wsMethod   = m
--        , wsParams   = ps
--        , wsIdent    = i
--        , wsInterval = iv
--        , wsComplete = False
--        , wsCancel   = c
--        }
--  parseJSON x = typeMismatch "WSRPC" x
