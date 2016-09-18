{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , RecordWildCards
  #-}

module Monero.Types where

import Data.Aeson as A
import Data.Aeson.Types as A
import qualified Data.Text as T



data Balance = Balance
  { balance         :: Int
  , unlockedBalance :: Int
  } deriving (Show, Eq)

instance FromJSON Balance where
  parseJSON (Object o) = do
    b <- o .: "balance"
    u <- o .: "unlocked_balance"
    pure $ Balance b u
  parseJSON x = typeMismatch "GotBalance" x


newtype Address = Address
  { getAddress :: T.Text
  } deriving (Show, Eq, FromJSON, ToJSON)


newtype PaymentId = PaymentId
  { getPaymentId :: T.Text
  } deriving (Show, Eq, FromJSON, ToJSON)


data Payment = Payment
  { paymentId          :: PaymentId
  , paymentTxHash      :: T.Text -- FIXME hash type
  , paymentAmount      :: Int
  , paymentBlockHeight :: Int
  , paymentUnlockTime  :: Int -- FIXME: Use Word32 etc
  } deriving (Show, Eq)

instance FromJSON Payment where
  parseJSON (Object o) =
    Payment <$> o .: "payment_id"
            <*> o .: "tx_hash"
            <*> o .: "amount"
            <*> o .: "block_height"
            <*> o .: "unlock_time"
  parseJSON x = typeMismatch "Payment" x


data TransferDestination = TransferDestination
  { destinationAmount  :: Int
  , destinationAddress :: Address
  } deriving (Show, Eq)

instance ToJSON TransferDestination where
  toJSON TransferDestination{..} = object
    [ "amount" .= destinationAmount
    , "address" .= destinationAddress
    ]

data Transfer = Transfer
  { transferAmount      :: Int -- FIXME uint
  , transferSpent       :: Bool
  , transferGlobalIndex :: Int
  , transferTxHash      :: T.Text -- FIXME hash type
  , transferTxSize      :: Int -- FIXME uint
  } deriving (Show, Eq)

instance FromJSON Transfer where
  parseJSON (Object o) =
    Transfer <$> o .: "amount"
             <*> o .: "spent"
             <*> o .: "global_index"
             <*> o .: "tx_hash"
             <*> o .: "tx_size"
  parseJSON x = typeMismatch "Transfer" x
