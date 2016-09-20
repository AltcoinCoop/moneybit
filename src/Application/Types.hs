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
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (Request)


-- * Infrastructure of the App


-- ** Read-Only Data

data Env = Env
  { envAuthority :: UrlAuthority
  , envWrkDir    :: FilePath
  , envStatic    :: FilePath
  } deriving (Show, Eq)


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



-- * Exceptions

data InitException
  = MalformedConfigFile LBS.ByteString
  deriving (Show, Eq, Generic)
instance Exception InitException

data ProcessException
  = NotEnoughHandles String
  deriving (Show, Eq, Generic)
instance Exception ProcessException
