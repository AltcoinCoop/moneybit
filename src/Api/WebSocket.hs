{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , FlexibleInstances
  #-}

module Api.WebSocket where

import Api.WebSocket.RPC
import Api

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch, Parser)
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


instance FromJSON (WSRPC (T.Text, OpenRequest)) where
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
      case ps of
        Object o' -> do
          ps' <- o' .: "open"
          w   <- o' .: "name"
          i  <- o .:  "id"
          c  <- o .:? "cancel" .!= False
          pure WSRPC
            { wsMethod   = m
            , wsParams   = (w,ps')
            , wsIdent    = i
            , wsInterval = iv
            , wsComplete = False
            , wsCancel   = c
            }
        _ -> fail "params not object"
  parseJSON x = typeMismatch "WSRPC" x

instance ToJSON (WSRPC T.Text) where
  toJSON WSRPC{..} = A.object
    [ "version"  .= version
    , "method"   .= ("new" :: T.Text)
    , "complete" .= True
    , "id"       .= wsIdent
    , "params"   .= wsParams
    ]

instance ToJSON (WSRPC NewProgress) where
  toJSON WSRPC{..} = A.object
    [ "version"  .= version
    , "method"   .= ("new" :: T.Text)
    , "complete" .= False
    , "id"       .= wsIdent
    , "params"   .= wsParams
    ]

instance ToJSON (WSRPC OpenProgress) where
  toJSON WSRPC{..} = A.object
    [ "version"  .= version
    , "method"   .= ("open" :: T.Text)
    , "complete" .= False
    , "id"       .= wsIdent
    , "params"   .= wsParams
    ]

instance ToJSON (WSRPC OpenResponse) where
  toJSON WSRPC{..} = A.object
    [ "version"  .= version
    , "method"   .= ("open" :: T.Text)
    , "complete" .= True
    , "id"       .= wsIdent
    , "params"   .= wsParams
    ]

newtype NewProgress = NewProgress Double
instance ToJSON NewProgress where
  toJSON (NewProgress x) = toJSON x
newtype OpenProgress = OpenProgress Double
instance ToJSON OpenProgress where
  toJSON (OpenProgress x) = toJSON x



data WSSubscribe
  = WSSubNew (WSRPC NewRequest)
  | WSSubOpen (WSRPC (T.Text, OpenRequest))
  | Pong

instance FromJSON WSSubscribe where
  parseJSON x = (WSSubNew  <$> parseJSON x)
            <|> (WSSubOpen <$> parseJSON x)
            <|> (Pong <$ (parseJSON x :: Parser ()))


data WSSupply

data WSReply
  = WSNewProgress (WSRPC NewProgress)
  | WSOpenProgress (WSRPC OpenProgress)

instance ToJSON WSReply where
  toJSON (WSNewProgress x) = toJSON x
  toJSON (WSOpenProgress x) = toJSON x

data WSComplete
  = WSComNew (WSRPC T.Text)
  | WSComOpen (WSRPC OpenResponse)

instance ToJSON WSComplete where
  toJSON (WSComNew x) = toJSON x
  toJSON (WSComOpen x) = toJSON x


data WSRPCIncomingCommand
  = WSSubscribe WSSubscribe
  | WSSupply WSSupply

instance FromJSON WSRPCIncomingCommand where
  parseJSON x = (WSSubscribe <$> parseJSON x)

data WSRPCOutgoingCommand
  = WSReply WSReply
  | WSComplete WSComplete

-- instance ToJSON WSRPCOutgoingCommand where ...
