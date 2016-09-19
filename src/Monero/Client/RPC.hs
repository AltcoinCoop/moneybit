{-# LANGUAGE
    FlexibleContexts
  , OverloadedStrings
  , RecordWildCards
  #-}

module Process.Client.RPC where

import Monero.Types
import Application.Types
import Data.Json.RPC

import Data.Aeson as A
import Data.Aeson.Types as A
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Network (HostName)
import Network.HTTP.Client (Manager)




getBalance :: MonadApp m => Int -> Manager -> m Balance
getBalance p m = rpc p m "getbalance" nada

newtype GotAddress = GotAddress Address
instance FromJSON GotAddress where
  parseJSON (Object o) = GotAddress <$> o .: "address"
  parseJSON x = typeMismatch "GotAddress" x

getAddress :: MonadApp m => Int -> Manager -> m GotAddress
getAddress p m = rpc p m "getaddress" nada

newtype GotHeight = GotHeight Int
instance FromJSON GotHeight where
  parseJSON (Object o) = GotHeight <$> o .: "height"
  parseJSON x = typeMismatch "GotHeight" x

getHeight :: MonadApp m => Int -> Manager -> m GotHeight
getHeight p m = rpc p m "getheight" nada


data MakeTransfer = MakeTransfer
  { makeTransferDestinations :: [TransferDestination]
  , makeTransferMixin        :: Int
  , makeTransferUnlockTime   :: Int
  , makeTransferPaymentId    :: Maybe PaymentId
  , makeTransferGetTxKey     :: Bool
  } deriving (Show, Eq)
  -- NOTE: fee is obsolete
instance ToJSON MakeTransfer where
  toJSON MakeTransfer{..} = object
    [ "destinations" .= makeTransferDestinations
    , "mixin"        .= makeTransferMixin
    , "unlock_time"  .= makeTransferUnlockTime
    , "payment_id"   .= makeTransferPaymentId
    , "get_tx_key"   .= makeTransferGetTxKey
    ]

data MadeTransfer = MadeTransfer
  { transactionHash :: T.Text -- FIXME to hash type?
  , transactionKey  :: Maybe T.Text -- FIXME transaction key
  } deriving (Show, Eq)
instance FromJSON MadeTransfer where
  parseJSON (Object o) =
    MadeTransfer <$> o .:  "tx_hash"
                 <*> o .:? "tx_key"
  parseJSON x = typeMismatch "MadeTransfer" x

transfer :: MonadApp m => Int -> Manager -> MakeTransfer -> m MadeTransfer
transfer p m t = rpc p m "transfer" $ Just t


data MakeTransferSplit = MakeTransferSplit
  { makeTransferSplitDestinations :: [TransferDestination]
  , makeTransferSplitMixin        :: Int
  , makeTransferSplitUnlockTime   :: Int
  , makeTransferSplitPaymentId    :: Maybe PaymentId
  , makeTransferSplitGetTxKey     :: Bool
  , makeTransferSplitNewAlgorithm :: Bool
  } deriving (Show, Eq)
  -- NOTE: fee is obsolete
instance ToJSON MakeTransferSplit where
  toJSON MakeTransferSplit{..} = object
    [ "destinations"  .= makeTransferSplitDestinations
    , "mixin"         .= makeTransferSplitMixin
    , "unlock_time"   .= makeTransferSplitUnlockTime
    , "payment_id"    .= makeTransferSplitPaymentId
    , "get_tx_key"    .= makeTransferSplitGetTxKey
    , "new_algorithm" .= makeTransferSplitNewAlgorithm
    ]

data MadeTransferSplit = MadeTransferSplit
  { transactionHashList :: [T.Text] -- FIXME no keys? FIXME hash type
  } deriving (Show, Eq)
instance FromJSON MadeTransferSplit where
  parseJSON (Object o) = MadeTransferSplit <$> o .: "tx_hash_list"
  parseJSON x = typeMismatch "MadeTransferSplit" x

transferSplit :: MonadApp m => Int -> Manager -> MakeTransferSplit -> m MadeTransferSplit
transferSplit p m t = rpc p m "transfer_split" $ Just t


data SweptDust = SweptDust
  { sweptTxHashList :: [T.Text] -- FIXME hash type
  } deriving (Show, Eq)
instance FromJSON SweptDust where
  parseJSON (Object o) = SweptDust <$> o .: "tx_hash_list"
  parseJSON x = typeMismatch "SweptDust" x

sweepDust :: MonadApp m => Int -> Manager -> m SweptDust
sweepDust p m = rpc p m "sweep_dust" nada


data EmptyObject = EmptyObject
instance FromJSON EmptyObject where
  parseJSON (Object o) | o == mempty = pure EmptyObject
                       | otherwise   = fail "not an empty object"
  parseJSON x = typeMismatch "EmptyObject" x

store :: MonadApp m => Int -> Manager -> m EmptyObject
store p m = rpc p m "store" nada


newtype GetPayments = GetPayments
  { getPaymentsId :: PaymentId
  } deriving (Show, Eq)
instance ToJSON GetPayments where
  toJSON GetPayments{..} = object ["payment_id" .= getPaymentsId]

newtype GotPayments = GotPayments
  { gotPayments :: [Payment]
  } deriving (Show, Eq)
instance FromJSON GotPayments where
  parseJSON (Object o) = GotPayments <$> o .: "payments"
  parseJSON x = typeMismatch "GotPayments" x

getPayments :: MonadApp m => Int -> Manager -> GetPayments -> m GotPayments
getPayments p m g = rpc p m "get_payments" $ Just g


data GetBulkPayments = GetBulkPayments
  { bulkPaymentIds            :: [PaymentId]
  , bulkPaymentMinBlockHeight :: Int -- FIXME unsigned
  } deriving (Show, Eq)
instance ToJSON GetBulkPayments where
  toJSON GetBulkPayments{..} = object
    [ "payment_ids" .= bulkPaymentIds
    , "min_block_height" .= bulkPaymentMinBlockHeight
    ]

newtype GotBulkPayments = GotBulkPayments
  { gotBulkPayments :: [Payment]
  } deriving (Show, Eq)
instance FromJSON GotBulkPayments where
  parseJSON (Object o) = GotBulkPayments <$> o .: "payments"
  parseJSON x = typeMismatch "GotBulkPayments" x

getBulkPayments :: MonadApp m => Int -> Manager -> GetBulkPayments -> m GotBulkPayments
getBulkPayments p m g = rpc p m "get_bulk_payments" $ Just g


data IncomingTransferType
  = AllTransferTypes
  | AvailableTransfers
  | UnavailableTransfers
  deriving (Show, Eq)
instance ToJSON IncomingTransferType where
  toJSON AllTransferTypes = String "all"
  toJSON AvailableTransfers = String "available"
  toJSON UnavailableTransfers = String "unavailable"

newtype GetIncomingTransfers = GetIncomingTransfers
  { getIncomingTransfers :: IncomingTransferType
  } deriving (Show, Eq)
instance ToJSON GetIncomingTransfers where
  toJSON GetIncomingTransfers{..} = object ["transfer_type" .= getIncomingTransfers]

newtype GotIncomingTransfers = GotIncomingTransfers
  { gotIncomingTransfers :: [Transfer]
  } deriving (Show, Eq)
instance FromJSON GotIncomingTransfers where
  parseJSON (Object o) = GotIncomingTransfers <$> o .:? "transfers" .!= [] -- Might not exist
  parseJSON x = typeMismatch "GotIncomingTransfers" x

incomingTransfers :: MonadApp m => Int -> Manager -> GetIncomingTransfers
                  -> m GotIncomingTransfers
incomingTransfers p m g = rpc p m "incoming_transfers" $ Just g


data QueryKeyType
  = KeyMnemonic
  | KeyView
  deriving (Show, Eq)
instance ToJSON QueryKeyType where
  toJSON KeyMnemonic = String "mnemonic"
  toJSON KeyView     = String "view_key"

newtype QueryKey = QueryKey
  { queryKeyType :: QueryKeyType
  } deriving (Show, Eq)
instance ToJSON QueryKey where
  toJSON QueryKey{..} = object ["key_type" .= queryKeyType]

{-
data QueriedKey
  = ViewKey Address
  | Mnemonic T.Text
  deriving (Show, Eq)
instance FromJSON QueriedKey where
  parseJSON (String s) -} -- FIXME attoparsec hexadecimal

queryKey :: MonadApp m => Int -> Manager -> QueryKey -> m T.Text -- FIXME account for either
queryKey p m q = rpc p m "query_key" $ Just q


newtype MakeIntegratedAddress = MakeIntegratedAddress
  { makeIntegratedAddress' :: Maybe PaymentId
  } deriving (Show, Eq)
instance ToJSON MakeIntegratedAddress where
  toJSON MakeIntegratedAddress{..} = object
    ["payment_id" .= fromMaybe "" makeIntegratedAddress']

newtype MadeIntegratedAddress = MadeIntegratedAddress
  { madeIntegratedAddress :: Address
  } deriving (Show, Eq)
instance FromJSON MadeIntegratedAddress where
  parseJSON (Object o) = MadeIntegratedAddress <$> o .: "integrated_address"
  parseJSON x = typeMismatch "MadeIntegratedAddress" x

makeIntegratedAddress :: MonadApp m => Int -> Manager -> MakeIntegratedAddress
                      -> m MadeIntegratedAddress
makeIntegratedAddress p m m' = rpc p m "make_integrated_address" $ Just m'


newtype SplitIntegratedAddress = SplitIntegratedAddress
  { splitIntegratedAddress' :: Address
  } deriving (Show, Eq)
instance ToJSON SplitIntegratedAddress where
  toJSON SplitIntegratedAddress{..} = object
    ["integrated_address" .= splitIntegratedAddress']

data GotSplitIntegratedAddress = GotSplitIntegratedAddress
  { gotSplitStandardAddress :: Address
  , gotSplitPaymentId       :: PaymentId
  } deriving (Show, Eq)
instance FromJSON GotSplitIntegratedAddress where
  parseJSON (Object o) =
    GotSplitIntegratedAddress <$> o .: "standard_address"
                              <*> o .: "payment_id"
  parseJSON x = typeMismatch "GotSplitIntegratedAddress" x
