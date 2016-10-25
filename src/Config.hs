{-# LANGUAGE
    RecordWildCards
  , OverloadedStrings
  , CPP
  #-}

module Config where

{-|

Just use a .json file for config, easy enough.

-}

import Data.Aeson as A
import Data.Default
import Network.URI (parseURI)
import Text.Hostname (validHostname)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


data Config = Config
  { walletConfig :: WalletConfig
  } deriving (Show, Eq)


instance Default Config where
  def = Config
    { walletConfig = def
    }

instance FromJSON Config where
  parseJSON (Object o) = do
    w <- o .: "wallet"
    pure Config
      { walletConfig = w
      }
  parseJSON _ = fail "Not an object"

instance ToJSON Config where
  toJSON Config{..} =
    object [ "wallet" .= toJSON walletConfig
           ]


data MoneroDNode
  = LocalNode
  | RemoteNode FilePath
  deriving (Show, Eq)

instance Default MoneroDNode where
  def = LocalNode

instance FromJSON MoneroDNode where
  parseJSON (String s) | s == "local" = pure LocalNode
  parseJSON (Object o) = do
    u <- o .: "url"
    if not . validHostname $ T.encodeUtf8 u
    then fail "Not a url"
    else pure . RemoteNode $ T.unpack u
  parseJSON _ = fail "Not a url or local"

instance ToJSON MoneroDNode where
  toJSON LocalNode = toJSON ("local" :: T.Text)
  toJSON (RemoteNode r) = object ["url" .= r]



-- | Represents the type of parsed configs
data WalletConfig = WalletConfig
  { walletCliPath     :: FilePath
  , walletMoneroDNode :: MoneroDNode
  } deriving (Show, Eq)

instance Default WalletConfig where
  def = WalletConfig
#ifdef mingw32_HOST_OS
    { walletCliPath = "monero-wallet-cli.exe"
#else
    { walletCliPath = "monero-wallet-cli"
#endif
    , walletMoneroDNode = def
    }

instance FromJSON WalletConfig where
  parseJSON (Object o) = do
    p <- o .: "monero-wallet-cli-path"
    n <- o .: "wallet-monerod-node"
    pure WalletConfig
      { walletCliPath = p
      , walletMoneroDNode = n
      }
  parseJSON _ = fail "Not an object"

instance ToJSON WalletConfig where
  toJSON WalletConfig {..} =
    object [ "monero-wallet-cli-path" .= toJSON walletCliPath
           , "wallet-monerod-node" .= toJSON walletMoneroDNode
           ]

