module Process.Client.RPC where

import Data.Json.RPC
import Data.Aeson as A



data GotBalance = GotBalance
  { balance :: Double
  , unlockedBalance :: Double
  }

getBalance :: MonadApp m => m ?
