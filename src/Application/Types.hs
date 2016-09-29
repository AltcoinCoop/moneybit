{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , MultiParamTypeClasses
  , DeriveGeneric
  , RecordWildCards
  #-}

module Application.Types where

import Data.Url
import Data.Typeable
import Data.Aeson as A
import Data.Aeson.Encode.Pretty as A hiding (Config)
import Path.Extended
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.State
import GHC.Generics
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base64 as BS64
import Network.HTTP.Client (Request)

import Crypto.Saltine.Core.Box (PublicKey, SecretKey)
import Crypto.Saltine.Class as NaCl


-- * Infrastructure of the App


-- ** Read-Only Data

data Env = Env
  { envAuthority :: UrlAuthority
  , envWrkDir    :: FilePath
  , envCertPk    :: PublicKey
  , envCertSk    :: SecretKey
  } deriving (Eq)

instance Show Env where
  show Env{..} = unlines
    [ "Env:  - host: " ++ showUrlAuthority envAuthority
    , "      - dir:  " ++ envWrkDir
    , "      - cert public key: " ++ show (BS64.encode $ NaCl.encode envCertPk)
    , "      - cert secret key: <###>"
    ]


-- ** Stateful Data

type Config = ()

-- | Update the config file every time it's changed in the UI
configure :: MonadApp m
          => FilePath
          -> (Config -> Config)
          -> m ()
configure cfgPath f = do
  Mutable{..} <- get
  let config' = f config
  put Mutable
        { config = config'
        , rpcId  = rpcId
        }
  liftIO . LBS.writeFile cfgPath
         $ A.encodePretty config'


data Mutable = Mutable
  { config :: Config
  , rpcId  :: Int
  } deriving (Show, Eq)

mkMutable :: Config -> Mutable
mkMutable c = Mutable
  { config = c
  , rpcId  = 0
  }


type AppM = LoggingT (ReaderT Env (StateT Mutable IO))

runAppM :: Env -> Mutable -> AppM a -> IO a
runAppM env mut xs = evalStateT (runReaderT (runStderrLoggingT xs) env) mut

type MonadApp m =
  ( MonadReader Env m
  , MonadState Mutable m
  , MonadIO m
  , MonadThrow m
  , MonadCatch m
  , MonadMask m
  )


-- * Links

data AppLinks
  = AppWallets

instance ToPath AppLinks Abs File where
  toPath AppWallets = parseAbsFile "/index"

instance ToLocation AppLinks Abs File where
  toLocation AppWallets = fromPath <$> toPath AppWallets


data AssetLinks
  = JQuery
  | SemanticCss
  | SemanticJs
  | Qrious
  | CryptoCoinsCss
  | CryptoCoinsColorsCss
  | ClipboardJs
  | ScryptJs
  | NaClJs
  | ZxcvbnJs

instance ToPath AssetLinks Abs File where
  toPath JQuery      = parseAbsFile "/static/jquery"
  toPath SemanticCss = parseAbsFile "/static/semantic/semantic"
  toPath SemanticJs  = parseAbsFile "/static/semantic/semantic"
  toPath Qrious      = parseAbsFile "/static/qrious"
  toPath CryptoCoinsCss = parseAbsFile "/static/cryptocoins/cryptocoins"
  toPath CryptoCoinsColorsCss = parseAbsFile "/static/cryptocoins/cryptocoins-colors"
  toPath ClipboardJs = parseAbsFile "/static/clipboard"
  toPath ScryptJs    = parseAbsFile "/static/scrypt"
  toPath NaClJs      = parseAbsFile "/static/nacl"
  toPath ZxcvbnJs    = parseAbsFile "/static/zxcvbn"

instance ToLocation AssetLinks Abs File where
  toLocation JQuery      = (addFileExt "min.js" . fromPath) <$> toPath JQuery
  toLocation SemanticCss = (addFileExt "css"    . fromPath) <$> toPath SemanticCss
  toLocation SemanticJs  = (addFileExt "js"     . fromPath) <$> toPath SemanticJs
  toLocation Qrious      = (addFileExt "js"     . fromPath) <$> toPath Qrious
  toLocation CryptoCoinsCss = (addFileExt "css" . fromPath) <$> toPath CryptoCoinsCss
  toLocation CryptoCoinsColorsCss = (addFileExt "css" . fromPath) <$> toPath CryptoCoinsColorsCss
  toLocation ClipboardJs = (addFileExt "js"     . fromPath) <$> toPath ClipboardJs
  toLocation ScryptJs    = (addFileExt "js"     . fromPath) <$> toPath ScryptJs
  toLocation NaClJs      = (addFileExt "js"     . fromPath) <$> toPath NaClJs
  toLocation ZxcvbnJs    = (addFileExt "js"     . fromPath) <$> toPath ZxcvbnJs



-- * Exceptions

data InitException
  = MalformedConfigFile LBS.ByteString
  deriving (Show, Eq, Generic)
instance Exception InitException

data ProcessException
  = NotEnoughHandles String
  deriving (Show, Eq, Generic)
instance Exception ProcessException

data ApiException
  = TranscodeDecodeError LBS.ByteString
  | TranscodeDecodeByteError BS.ByteString
  deriving (Show, Eq, Generic)
instance Exception ApiException
