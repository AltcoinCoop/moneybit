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


