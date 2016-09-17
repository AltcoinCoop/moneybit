module Process.Client.RPC where

import Data.Json.RPC
import Network (HostName)
import Network.HTTP.Client (Manager)


data GotBalance = GotBalance
  { balance         :: Double
  , unlockedBalance :: Double
  } deriving (Show, Eq)

getBalance :: Manager -> HostName -> IO (RPCResponse GotBalance)
getBalance m h = rpc m h $ RPCRequest
