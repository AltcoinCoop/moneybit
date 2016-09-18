{-# LANGUAGE
    DeriveGeneric
  , NamedFieldPuns
  , RecordWildCards
  , CPP
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
import System.Environment (lookupEnv)
import System.Directory ( doesFileExist, doesDirectoryExist
                        , getHomeDirectory, createDirectoryIfMissing
                        , getAppUserDataDirectory)
import System.IO.Error (isDoesNotExistError)
import System.FilePath
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#endif
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
          , config = Nothing
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
       <> help "config file location - default `~/.moneybit/moneybit.json`"



-- | Note that this function will fail to pattern match on @Nothing@'s - use
-- @def@ beforehand.
digestAppOpts :: AppOpts -> IO (Env, Config)
digestAppOpts AppOpts
               { port = Just p
               , config = mc
               } = do
  let a = UrlAuthority
            { urlScheme  = "http"
            , urlSlashes = True
            , urlAuth    = Nothing
            , urlHost    = "localhost"
            , urlPort    = p <$ guard (p /= 80)
            }
  wrkDir <- getAppUserDataDirectory "moneybit" `catch`
    (\e ->  if not $ isDoesNotExistError e
            then throwM e
            else do
#if defined(mingw32_HOST_OS)
              home <- Win32.sHGetFolderPath nullPtr Win32.cSIDL_APPDATA nullPtr 0
              let x = home ++ "\\moneybit"
#else
              home <- getHomeDirectory
              let x = home ++ "/.moneybit"
#endif
              return x
    )

  createDirectoryIfMissing True wrkDir

  c <-
    case mc of
      Nothing ->
        pure $ wrkDir </> "moneybit.json"
      Just c -> pure c

  exists <- doesFileExist c
  cfg <- if exists
         then do
            cfgFile <- LBS.readFile c
            case A.decode cfgFile of
              Nothing  -> throwM $ MalformedConfigFile cfgFile
              Just cfg -> pure cfg
         else do
            putStrLn $ "No config found, writing to " ++ c
            LBS.writeFile c $ A.encodePretty (def :: Config)
            pure def

  pure ( Env
           { envAuthority = a
           , envWrkDir = wrkDir
           }
       , cfg
       )
digestAppOpts AppOpts{..} = error "impossible state"


runAppMTest :: AppM a -> IO a
runAppMTest xs = do
  (e,c) <- digestAppOpts def
  runAppM e (mkMutable c) xs
