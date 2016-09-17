{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , MultiParamTypeClasses
  , DeriveGeneric
  #-}

module Application.Types where

import Config
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


-- * Infrastructure of the App


-- ** Read-Only Data

data Env = Env
  { envAuthority :: UrlAuthority
  , envWrkDir    :: FilePath
  } deriving (Show, Eq)



-- | Update the config file every time it's changed in the UI
configure :: MonadApp m
          => FilePath
          -> (Config -> Config)
          -> m ()
configure cfgPath f = do
  cfg <- get
  let cfg' = f cfg
  put cfg'
  liftIO . LBS.writeFile cfgPath $ A.encodePretty cfg'




type AppM = LoggingT (ReaderT Env (StateT Config IO))

runAppM :: Env -> Config -> AppM a -> IO a
runAppM env cfg xs = evalStateT (runReaderT (runStderrLoggingT xs) env) cfg

type MonadApp m =
  ( MonadReader Env m
  , MonadState Config m
  , MonadIO m
  , MonadThrow m
  , MonadCatch m
  , MonadMask m
  )


-- * Links

data AppLinks
  = AppHome

instance ToPath AppLinks Abs File where
  toPath AppHome = parseAbsFile "/index"

instance ToLocation AppLinks Abs File where
  toLocation AppHome = (addFileExt "min.js" . fromPath) <$> toPath AppHome



-- * Exceptions

data InitException
  = MalformedConfigFile LBS.ByteString
  deriving (Show, Eq, Generic)
instance Exception InitException

data ProcessException
  = NotEnoughHandles String
  deriving (Show, Eq, Generic)
instance Exception ProcessException

data RPCException
  = MalformedRPCData LBS.ByteString
  deriving (Show, Eq, Generic)
instance Exception RPCException
