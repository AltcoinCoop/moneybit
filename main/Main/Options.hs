{-# LANGUAGE
    DeriveGeneric
  #-}

module Main.Options where

import Application.Types

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import Data.Default
import Data.Monoid
import Data.Url
import Control.Monad
import GHC.Generics


-- | Application-wide options
data AppOpts = AppOpts
  { port   :: Maybe Int
  , host   :: Maybe String
  , cwd    :: Maybe FilePath
  , static :: Maybe FilePath
  } deriving Generic

instance Monoid AppOpts where
  mempty = AppOpts Nothing Nothing Nothing Nothing
  (AppOpts p h c s) `mappend` (AppOpts p' h' c' s') =
    AppOpts
      (getLast $ Last p <> Last p')
      (getLast $ Last h <> Last h')
      (getLast $ Last c <> Last c')
      (getLast $ Last s <> Last s')

instance Y.ToJSON AppOpts where
  toJSON = A.genericToJSON A.defaultOptions

instance Y.FromJSON AppOpts where
  parseJSON = A.genericParseJSON A.defaultOptions

instance Default AppOpts where
  def = AppOpts (Just 3000) (Just "http://localhost") (Just ".") (Just "./static")

appOpts :: Parser AppOpts
appOpts = AppOpts <$> portOpt <*> hostOpt <*> cwdOpt <*> staticOpt
  where
    portOpt = optional . option auto $
          long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on"
    hostOpt = optional . strOption $
          long "host"
       <> short 'h'
       <> metavar "HOST"
       <> help "host to deploy URLs over"
    cwdOpt = optional . strOption $
          long "cwd"
       <> short 'c'
       <> metavar "CWD"
       <> help "directory to search for files"
    staticOpt = optional . strOption $
          long "static"
       <> short 's'
       <> metavar "STATIC"
       <> help "directory to serve static files"


-- | Command-line options
data App = App
  { options    :: AppOpts
  , configPath :: Maybe String
  }

app :: Parser App
app = App
  <$> appOpts
  <*> optional ( strOption
        ( long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> help "path to config file" ))



-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
appOptsToEnv :: AppOpts -> IO Env
appOptsToEnv (AppOpts (Just p) (Just h) (Just c) (Just s)) = do
  let a = UrlAuthority
            { urlScheme  = "http"
            , urlSlashes = True
            , urlAuth    = Nothing
            , urlHost    = h
            , urlPort    = p <$ guard (p /= 80)
            }
  pure $ Env a c s
appOptsToEnv (AppOpts _ _ _ _) = error "impossible state"
