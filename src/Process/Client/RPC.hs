{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  #-}

module Process.Client.RPC where

import Application.Types
import Data.Json.RPC

import Data.Aeson as A
import Data.Aeson.Types as A
import Network (HostName)
import Network.HTTP.Client (Manager)



data GotBalance = GotBalance
  { balance         :: Double
  , unlockedBalance :: Double
  } deriving (Show, Eq)

instance FromJSON GotBalance where
  parseJSON (Object o) = do
    b <- o .: "balance"
    u <- o .: "unlocked_balance"
    pure $ GotBalance b u
  parseJSON x = typeMismatch "GotBalance" x


getBalance :: MonadApp m => Int -> Manager -> m (RPCResponse GotBalance)
getBalance p m = rpc p m "getbalance" nil
