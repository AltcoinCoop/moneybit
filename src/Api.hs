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


-- * WebSockets

{-

# WebSocket RPC Spec

- Instead of `request -> response`, you have `(subscribe / supply) -> (reply / complete)`

* A subscription may include an optional `interval :: Millisecond` flag to restrain replies, given
  they're routine in nature.

* This model assumes a _streaming_ extension of the traditional `request -> response` model - each
  RPC is more of a line than a dot, where each session of an individual RPC is uniquely identified
  and replies / supplies may occur at any asynchronous point on that line (which represents time as
  a Real number - as per Conal Eliot's denotational time programming semantics, although this does
  not introduce behaviors and just sticks with events - this is just RPC, not FRP).

Generally speaking, each frame of a WS-RPC looks as follows:

- WS-RPC
  - version  :: SemVar
  - id       :: String
  - interval :: Maybe Millisecond
  - method   :: k
  - params   :: k

Where in every subscription, a unique id should be chosen by the subscriber for their own bookkeeping,
and to let the server know what can be overwritten / garbage collected. Invariants assume unique ids.

Also, the `method` and `param` fields are left represented as `k` because they're so similar in their use;
the types defined below will suit the different roles of `params` while their type name could be placed in
`method`.

Default intervals will be chosen for each endpoint in a domain-specific style if one isn't provided.

Some WS-RPC channels will never complete, and have no interval - all messages incoming are asynchronous;
this is the case for transaction history updates.


If data is supplied to a method that isn't expected, the session / server should ignore it.

Every session also features a specific `cancel` option when issuing a `supply` - just throw in a `cancel : True` flag
in the RPC header.


This session-like RPC model is more general than the traditional `request -> response` model - to lift a simple blocking
RPC, format it like this:

- subscribe :: request
- supply    :: n/a
- reply     :: n/a
- complete  :: response


# Monero WS-RPC

- New
  - subscribe
    - name     :: String
    - password :: String
    - language :: Language
    - mnemonic :: Maybe String
  - supply :: n/a
  - reply
    - progress :: Percent
  - complete
    - mnemonic :: String

- Open
  - subscribe
    - name            :: String
    - password        :: String
    - sessionPassword :: String
  - supply :: n/a
  - reply
    - progress :: Percent
  - complete :: ()

- Wallet / _name_

  - Transactions
    - subscribe :: ()
    - supply :: n/a
    - reply
      - value     :: Double
      - paymentId :: Maybe PaymentId
      - date      :: Date
    - complete :: n/a

    * confirmation
        - subscribe
          - paymentId :: PaymentId
        - supply   :: n/a
        - reply    :: () -- increment confirmations counter
        - complete :: n/a

  -- Simple lift from blocking model
  - Send
    - subscribe
      - recipient :: Address
      - paymentId :: Maybe String
      - amount    :: Double
      - mixin     :: Word8
    - reply :: n/a
    - supply :: n/a
    - complete :: ()

  - Seeds
    - subscribe :: ()
    - supply :: n/a
    - reply :: n/a
    - complte
      - mnemonic :: String
      - viewkey  :: String
      - spendkey :: String

-}
