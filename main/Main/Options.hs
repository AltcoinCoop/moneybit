{-# LANGUAGE
    DeriveGeneric
  , NamedFieldPuns
  , RecordWildCards
  #-}

module Main.Options where

import Application.Types
import Config

import           Options.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Monoid
import Data.Url
import Control.Monad
import Control.Monad.Catch
import System.Directory (doesFileExist)
import GHC.Generics


-- | Application-wide options
data AppOpts = AppOpts
  { port   :: Maybe Int
  , config :: Maybe FilePath
  } deriving Generic

instance Monoid AppOpts where
  mempty =
    AppOpts
      { port = Nothing
      , config = Nothing
      }
  mappend
    AppOpts
      { port = p1
      , config = c1
      }
    AppOpts
      { port = p2
      , config = c2
      } =
    AppOpts
      { port   = getLast $ Last p1 <> Last p2
      , config = getLast $ Last c1 <> Last c2
      }

instance Default AppOpts where
  def = AppOpts
          { port   = Just 3000
          , config = Just "./moneybit.json"
          }

appOpts :: Parser AppOpts
appOpts = AppOpts <$> portOpt <*> configOpt
  where
    portOpt = optional . option auto $
          long "port"
       <> short 'p'
       <> metavar "PORT"
       <> help "port to listen on - default `3000`"
    configOpt = optional . strOption $
          long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> help "config file location - default `./moneybit.json`"



-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
digestAppOpts :: AppOpts -> IO (Env, Config)
digestAppOpts AppOpts
               { port = Just p
               , config = Just c
               } = do
  let a = UrlAuthority
            { urlScheme  = "http"
            , urlSlashes = True
            , urlAuth    = Nothing
            , urlHost    = "localhost"
            , urlPort    = p <$ guard (p /= 80)
            }

  exists <- doesFileExist c
  cfg <- if exists
         then do
            cfgFile <- LBS.readFile c
            case A.decode cfgFile of
              Nothing  -> throwM $ MalformedConfigFile cfgFile
              Just cfg -> pure cfg
         else do
            putStrLn "No config found, writing to ./moneybit.json"
            LBS.writeFile c $ A.encodePretty (def :: Config)
            pure def

  pure ( Env
           { envAuthority = a
           }
       , cfg
       )
digestAppOpts AppOpts{..} = error "impossible state"
