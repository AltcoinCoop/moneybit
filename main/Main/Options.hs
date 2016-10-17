{-# LANGUAGE
    DeriveGeneric
  , NamedFieldPuns
  , RecordWildCards
  , CPP
  , TemplateHaskell
  #-}

module Main.Options where

import Application.Types

import           Options.Applicative
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Default
import Data.Monoid
import Data.Url
import Data.STRef
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map.Strict as Map
import Control.Monad
import Control.Monad.Catch
import Control.Monad.ST
import System.Directory ( doesFileExist
                        , getHomeDirectory, createDirectoryIfMissing
                        , getAppUserDataDirectory, getDirectoryContents)
import System.IO.Error (isDoesNotExistError)
import System.FilePath
#ifdef mingw32_HOST_OS
import qualified System.Win32 as Win32
#endif
import GHC.Generics
import Crypto.Saltine.Core.Box (newKeypair)
import Crypto.Saltine.Class as NaCl
import Language.Haskell.TH (runIO)


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
              let x = home </> "moneybit"
#else
              home <- getHomeDirectory
              let x = home </> ".moneybit"
#endif
              return x
    )

  createDirectoryIfMissing True wrkDir

  c <- case mc of
          Nothing -> pure $ wrkDir </> "config.json"
          Just c  -> pure c

  exists <- doesFileExist c
  cfg <- if exists
         then do
            cfgFile <- LBS.readFile c
            case A.decode cfgFile of
              Nothing   -> throwM $ MalformedConfigFile cfgFile
              Just cfg' -> pure cfg'
         else do
            putStrLn $ "No config found, writing to " ++ c

            createDirectoryIfMissing True $ wrkDir </> "wallets"
            ws <- getDirectoryContents $ wrkDir </> "wallets"
            let isNotWalletFile w = ".keys" `isSuffixOf` w
                                 || ".address.txt" `isSuffixOf` w
                                 || ".log" `isSuffixOf` w
                                 || "." == w
                                 || ".." == w
                                 || "$" `isPrefixOf` w

                cfg' = def { configWalletsPath = wrkDir </> "wallets"
                           , configWallets     = filter (not . isNotWalletFile) ws
                           }
            LBS.writeFile c $ A.encodePretty cfg'
            pure cfg'

  createDirectoryIfMissing True $ configWalletsPath cfg

  (certPk,certSk) <- case ( NaCl.decode $ unUnEncode $ fst certKeypair
                          , NaCl.decode $ unUnEncode $ snd certKeypair
                          ) of
                      (Just pk', Just sk') -> pure (pk',sk')
                      _                    -> error "impossible"
  (instSk,instPk) <- newKeypair

  let verifyWallet w = do
        e <- doesFileExist $ configWalletsPath cfg </> w
        unless e $ throwM $ NonexistentWallet $ configWalletsPath cfg </> w
  mapM_ verifyWallet $ configWallets cfg

  openWallets <- stToIO $ newSTRef Map.empty
  wsSessions  <- stToIO $ newSTRef Map.empty

  pure ( Env
           { envAuthority   = a
           , envWrkDir      = wrkDir
           , envCertPk      = certPk
           , envCertSk      = certSk
           , envInstPk      = instPk
           , envInstSk      = instSk
           , envOpenWallets = openWallets
           , envWSSessions  = wsSessions
           }
       , cfg
       )
digestAppOpts AppOpts{..} = error "impossible state"


runAppMTest :: AppM a -> IO a
runAppMTest xs = do
  (e,c) <- digestAppOpts def
  runAppM e (mkMutable c) xs



unUnEncode :: String -> BS.ByteString
unUnEncode x = case BS64.decode . T.encodeUtf8 $ T.pack x of
  Right y -> y
  _       -> error "impossible"


certKeypair :: (String,String)
certKeypair = $(do (pk,sk) <- runIO newKeypair
                   let unEncode :: BS.ByteString -> String
                       unEncode = T.unpack . T.decodeUtf8 . BS64.encode
                       xs = ( unEncode $ NaCl.encode pk
                            , unEncode $ NaCl.encode sk
                            )
                   [|xs|]
               )
