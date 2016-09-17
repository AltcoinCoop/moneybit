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
import Path.Extended
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger
import Control.Monad.State
import GHC.Generics
import qualified Data.ByteString.Lazy as LBS


-- * Infrastructure of the App

-- The environment accessible from our application
data Env = Env
  { envAuthority :: UrlAuthority
  } deriving (Show, Eq)


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
