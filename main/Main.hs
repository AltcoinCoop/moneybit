{-# LANGUAGE
    ScopedTypeVariables
  , OverloadedStrings
  , QuasiQuotes
  , TemplateHaskell
  #-}

module Main where

import Main.Options
-- import Config
import Application
import Application.Types
import Monero.Wallet.Process (closeWallet)

import Options.Applicative
import Network.Wai.Trans
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger

import System.Remote.Monitoring
import System.Process (readCreateProcess, shell)

import Data.Maybe
import Data.STRef
import Data.Strict.Tuple
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.ST
import Control.Monad.Catch
import Control.Exception (AsyncException)

import Text.Heredoc (there)
import Language.Haskell.TH


logo :: String
logo = [there|logo.txt|]


commit :: String
commit = $(do c <- runIO $ readCreateProcess (shell "git log -n 1") ""
              [|c|]
          )



main :: IO ()
main = do
  putStrLn $ unlines
    [ logo
    , "Build Commit:"
    , commit
    ]

  -- CLI Opts
  let opts :: ParserInfo AppOpts
      opts = info (helper <*> appOpts) $
          fullDesc
       <> progDesc "Run the HTTP wallet app over PORT with CONFIG."
       <> header "moneybit - a simple monero wallet"

  commandOpts <- execParser opts

  let cliOpts :: AppOpts
      cliOpts = def <> commandOpts

  (env,cfg) <- digestAppOpts cliOpts
  print env
  handle (catchInterrupt env)
    $ entry (fromJust $ port cliOpts) env (mkMutable cfg)


catchInterrupt :: Env -> AsyncException -> IO ()
catchInterrupt env e = do
  wallets <- stToIO $ readSTRef $ envOpenWallets env
  forM_ (Map.elems wallets) $ \(_ :!: hs) -> closeWallet hs
  -- should block
  throwM e


-- | Entry point, post options parsing
entry :: Int -> Env -> Mutable -> IO ()
entry p env mut = do
  forkServer "localhost" (p-1)
  run p $
      gzip def
    . logStdoutDev
    $ application
  where
    application = runApplicationT (runAppM env mut) app
   -- otherLogger app req resp = do
   --   print $ pathInfo req
   --   app req resp
