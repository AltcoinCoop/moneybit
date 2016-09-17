{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , DeriveGeneric
  , GADTs
  , StandaloneDeriving
  , FlexibleContexts
  #-}

module Data.Json.RPC where

import Application.Types

import Data.Aeson as A
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.IO.Class
import Network (HostName)
import Network.HTTP.Client ( Manager, httpLbs, parseUrl, RequestBody (RequestBodyLBS)
                           , requestBody, requestHeaders, method, responseBody
                           , Request
                           )
import GHC.Generics


data RPCRequest ps = RPCRequest
  { rpcReqMethod  :: T.Text -- ^ User Data
  , rpcReqParams  :: Maybe ps
  , rpcReqId      :: Int -- ^ Hardcode for our threading
  } deriving (Show, Eq)

instance ToJSON ps => ToJSON (RPCRequest ps) where
  toJSON RPCRequest{..} =
    object $
      [ "jsonrpc" .= ("2.0" :: T.Text)
      , "method"  .= rpcReqMethod
      , "id"      .= rpcReqId
      ] ++ case rpcReqParams of
             Nothing -> []
             Just ps -> ["params" .= ps]


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
  { rpcRespId      :: Int
  , rpcRespResult  :: Either RPCResponseError rs
  } deriving (Show, Eq)

instance FromJSON rs => FromJSON (RPCResponse rs) where
  parseJSON (Object o) = do
    v <- o .: "jsonrpc"
    if v /= ("2.0" :: T.Text)
    then fail "Not 2.0 version"
    else do
      i <- o .: "id"
      r <-  (Left <$> o .: "error")
        <|> (Right <$> o .: "result")
      pure $ RPCResponse i r
  parseJSON _ = fail "Not an object"



rpc :: ( ToJSON ps
       , Show ps
       , FromJSON rs
       , MonadApp m
       ) => Int -- ^ Port number to bind to
         -> Manager
         -> T.Text -- ^ Method
         -> Maybe ps
         -> m rs
rpc port mgr method mx = do
  let host = "127.0.0.1:" ++ show port -- always calling simplewallet from local box
  r   <- parseUrl $ "http://" ++ host ++ "/json_rpc"
  idx <- freshId
  let req = RPCRequest
              { rpcReqMethod = method
              , rpcReqParams = mx
              , rpcReqId     = idx
              }
      r' = r
              { requestHeaders = [("Content-Type","application/json")]
              , requestBody = RequestBodyLBS $ A.encode req
              , method = "POST"
              }
  response <- liftIO $ httpLbs r' mgr
  case A.decode $ responseBody response of
    Nothing -> throwM . MalformedRPCData $ responseBody response
    Just resp -> do
      let idx' = rpcRespId resp
      if idx' /= idx
      then throwM $ MismatchingIds idx idx' req
      else case rpcRespResult resp of
        Left e -> throwM $ RPCError e
        Right y -> pure y


freshId :: MonadApp m => m Int
freshId = do
  idx <- rpcId <$> get
  modify' (\m -> m {rpcId = rpcId m + 1})
  pure idx




data RPCException where
  MalformedRPCData :: LBS.ByteString -> RPCException
  MismatchingIds :: Show a => Int -> Int -> RPCRequest a -> RPCException
  RPCError :: RPCResponseError -> RPCException

deriving instance Show RPCException
-- deriving instance Eq RPCException
-- deriving instance Generic RPCException
instance Exception RPCException

