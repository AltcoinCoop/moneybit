{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  #-}

module Data.Json.RPC where

import Application.Types (RPCException (MalformedRPCData))

import Data.Aeson as A
import qualified Data.Text as T
import Control.Applicative
import Control.Monad.Catch
import Network (HostName)
import Network.HTTP.Client (Manager, httpLbs, parseUrl, RequestBody (RequestBodyLBS)
                           , requestBody, requestHeaders, method, responseBody)


data RPCRequest ps = RPCRequest
  { rpcReqVersion :: T.Text
  , rpcReqMethod  :: T.Text -- ^ User Data
  , rpcReqParams  :: Maybe ps
  , rpcReqId      :: Int -- ^ Hardcode for our threading
  } deriving (Show, Eq)

instance ToJSON ps => ToJSON (RPCRequest ps) where
  toJSON RPCRequest{..} =
    object
      [ "jsonrpc" .= rpcReqVersion
      , "method"  .= rpcReqMethod
      , "params"  .= rpcReqParams
      , "id"      .= rpcReqId
      ]


data RPCResponseError = RPCResponseError
  { rpcErrorCode    :: Int
  , rpcErrorMessage :: T.Text
  , rpcErrorData    :: Maybe A.Value
  } deriving (Show, Eq)

instance FromJSON RPCResponseError where
  parseJSON (Object o) = do
    c <- o .: "code"
    m <- o .: "message"
    d <- (o .: "data") <|> pure Nothing
    pure $ RPCResponseError c m d
  parseJSON _ = fail "Not an object"

data RPCResponse rs = RPCResponse
  { rpcRespVersion :: T.Text
  , rpcRespId      :: Int
  , rpcRespResult  :: Either RPCResponseError rs
  } deriving (Show, Eq)

instance FromJSON rs => FromJSON (RPCResponse rs) where
  parseJSON (Object o) = do
    v <- o .: "jsonrpc"
    i <- o .: "id"
    r <- (Left <$> o .: "error")
     <|> (Right <$> o .: "result")
    pure $ RPCResponse v i r
  parseJSON _ = fail "Not an object"



rpc :: ( ToJSON ps
       , FromJSON rs
       ) => Manager
         -> HostName
         -> RPCRequest ps
         -> IO (RPCResponse rs)
rpc mgr host req = do
  r <- parseUrl $ "http://" ++ host ++ "/json_rpc"
  let r' = r
         { requestHeaders = [("Content-Type","application/json")]
         , requestBody = RequestBodyLBS $ A.encode req
         , method = "POST"
         }
  response <- httpLbs r' mgr
  case A.decode $ responseBody response of
    Nothing -> throwM . MalformedRPCData $ responseBody response
    Just x  -> pure x
