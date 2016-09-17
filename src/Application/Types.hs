{-# LANGUAGE
    OverloadedStrings
  , ConstraintKinds
  , FlexibleContexts
  , MultiParamTypeClasses
  #-}

module Application.Types where

import Data.Url
import Data.Typeable
import Path.Extended
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Logger


-- * Infrastructure of the App

-- The environment accessible from our application
data Env = Env
  { envAuthority :: UrlAuthority
  , envCwd       :: FilePath
  , envStatic    :: FilePath
  } deriving (Show, Eq)


type AppM = LoggingT (ReaderT Env IO)

runAppM :: Env -> AppM a -> IO a
runAppM env xs = runReaderT (runStderrLoggingT xs) env

type MonadApp m =
  ( MonadReader Env m
  , MonadIO m
  , MonadThrow m
  , MonadCatch m
  , Typeable m
  )


-- * Links

data AppLinks
  = AppHome

instance ToPath AppLinks Abs File where
  toPath AppHome = parseAbsFile "/index"

instance ToLocation AppLinks Abs File where
  toLocation AppHome = (addFileExt "min.js" . fromPath) <$> toPath AppHome

