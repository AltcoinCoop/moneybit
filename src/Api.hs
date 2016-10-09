{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  #-}

module Api where

import qualified Monero.Wallet.Process as M
import qualified Monero.Types as M

import Data.Aeson as A
import Data.Aeson.Types (typeMismatch)
import Data.Time (UTCTime)
import Data.Word (Word8, Word64)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS



-- * Transcoding

data SupportedBases
  = Base16
  | Base64
  | Base58
  deriving (Show, Eq)

instance FromJSON SupportedBases where
  parseJSON (String s) | s == "base16" = pure Base16
                       | s == "base64" = pure Base64
                       | s == "base58" = pure Base58
                       | otherwise = fail "not a prescribed string"
  parseJSON x = typeMismatch "SupportedBases" x


data TranscodeRequest = TranscodeRequest
  { transcodeFrom  :: SupportedBases
  , transcodeTo    :: SupportedBases
  , transcodeInput :: BS.ByteString
  } deriving (Show, Eq)

instance FromJSON TranscodeRequest where
  parseJSON (Object o) =
    TranscodeRequest <$> o .: "from"
                     <*> o .: "to"
                     <*> (T.encodeUtf8 <$> o .: "input")
  parseJSON x = typeMismatch "TranscodeRequest" x


-- * New

data NewRequest = NewRequest
  { newName     :: T.Text
  , newPassword :: T.Text -- ^ Plaintext
  , newLanguage :: Language
  , newMnemonic :: Maybe T.Text -- ^ For recovery
  } deriving (Show, Eq)

instance FromJSON NewRequest where
  parseJSON (Object o) =
    NewRequest <$> o .: "name"
               <*> o .: "password"
               <*> o .: "language"
               <*> o .:? "mnemonic"
  parseJSON x = typeMismatch "NewRequest" x

instance ToJSON NewRequest where
  toJSON NewRequest{..} = object $
    [ "name"     .= newName
    , "password" .= newPassword
    , "language" .= newLanguage
    ] ++ case newMnemonic of
           Nothing -> []
           Just m  -> ["mnemonic" .= m]



-- * Send

data SendRequest = SendRequest
  { sendRecipient :: M.Address
  , sendAmount    :: Double
  , sendMixin     :: Word8
  , sendPaymentId :: Maybe M.PaymentId -- FIXME integrated
  } deriving (Show, Eq)

instance FromJSON SendRequest where
  parseJSON (Object o) =
    SendRequest <$> o .: "recipient"
                <*> o .: "amount"
                <*> o .: "mixin"
                <*> o .:? "paymentId"
  parseJSON x = typeMismatch "SendRequest" x


-- data SendResponse = SendResponse
--   { sendTransactions :: [(TransactionId, Double)]
--   }


-- * Open

data OpenRequest = OpenRequest
  { openPassword        :: T.Text
  , openSessionPassword :: T.Text
  } deriving (Show, Eq)

instance FromJSON OpenRequest where
  parseJSON (Object o) =
    OpenRequest <$> o .: "password"
                <*> o .: "sessionPassword"
  parseJSON x = typeMismatch "OpenRequest" x

instance ToJSON OpenRequest where
  toJSON OpenRequest{..} = object
    [ "password" .= openPassword
    , "sessionPassword" .= openSessionPassword
    ]


data OpenResponse = OpenResponse
  { openBalance     :: Balance
  , openAddress     :: M.Address
  , openHistory     :: [Transaction]
  , openHistoryMore :: Bool
  } deriving (Show, Eq)

instance ToJSON OpenResponse where
  toJSON OpenResponse{..} = object
    [ "balance"     .= openBalance
    , "address"     .= openAddress
    , "history"     .= openHistory
    , "historyMore" .= openHistoryMore
    ]


-- * History

-- | Represents the filter data
data HistoryRequest = HistoryRequest
  { historySent      :: Bool
  , historyReceived  :: Bool
  , historyLabel     :: Maybe T.Text
  , historyTxId      :: Maybe M.HexString
  , historyPaymentId :: Maybe M.PaymentId
  } deriving (Show, Eq)

instance FromJSON HistoryRequest where
  parseJSON (Object o) =
    HistoryRequest <$> o .: "sent"
                   <*> o .: "received"
                   <*> o .:? "label"
                   <*> o .:? "txId"
                   <*> o .:? "paymentId"
  parseJSON x = typeMismatch "HistoryRequest" x

data HistoryResponse = HistoryResponse
  { historyHistory :: [Transaction]
  , historyMore    :: Bool
  } deriving (Show, Eq)

instance ToJSON HistoryResponse where
  toJSON HistoryResponse{..} = object
    [ "history" .= historyHistory
    , "historyMore" .= historyMore
    ]


-- * Seeds

data SeedsResponse = SeedsResponse
  { seedsMnemonic :: T.Text
  , seedsViewkey  :: M.HexString
  , seedsSpendkey :: M.HexString
  } deriving (Show, Eq)

instance ToJSON SeedsResponse where
  toJSON SeedsResponse{..} = object
    [ "mnemonic" .= seedsMnemonic
    , "viewkey"  .= seedsViewkey
    , "spendkey" .= seedsSpendkey
    ]


-- * Misc

data Balance = Balance
  { balanceBalance  :: Double
  , balanceUnlocked :: Double
  } deriving (Show, Eq)

instance ToJSON Balance where
  toJSON Balance{..} = object
    [ "balance" .= balanceBalance
    , "unlocked" .= balanceUnlocked
    ]


data Transaction = Transaction
  { transactionValue         :: Double
  , transactionDate          :: UTCTime
  , transactionTxId          :: M.HexString
  , transactionConfirmations :: Word64
  } deriving (Show, Eq)

instance ToJSON Transaction where
  toJSON Transaction{..} = object
    [ "value"         .= transactionValue
    , "date"          .= transactionDate
    , "txId"          .= transactionTxId
    , "confirmations" .= transactionConfirmations
    ]


data Language
  = English
  | Spanish
  | German
  | Italian
  | Portuguese
  | Russian
  | Japanese
  deriving (Show, Eq)

instance FromJSON Language where
  parseJSON (String s)
    | s == "en" = pure English
    | s == "es" = pure Spanish
    | s == "de" = pure German
    | s == "it" = pure Italian
    | s == "pt" = pure Portuguese
    | s == "ru" = pure Russian
    | s == "ja" = pure Japanese
    | otherwise = fail "Not a language string"
  parseJSON x = typeMismatch "Language" x

instance ToJSON Language where
  toJSON l = case l of
    English     -> "en"
    Spanish     -> "es"
    German      -> "de"
    Italian     -> "it"
    Portuguese  -> "pt"
    Russian     -> "ru"
    Japanese    -> "ja"


toMoneroLanguage :: Language -> M.WalletLanguage
toMoneroLanguage l =
  case l of
    English    -> M.English
    Spanish    -> M.Spanish
    German     -> M.German
    Italian    -> M.Italian
    Portuguese -> M.Portuguese
    Russian    -> M.Russian
    Japanese   -> M.Japanese


toMoneroAmount :: Double -> Word64
toMoneroAmount x = floor $ x * 1e12

fromMoneroAmount :: Word64 -> Double
fromMoneroAmount x = fromIntegral x / 1e12
