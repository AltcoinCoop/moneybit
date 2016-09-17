{-# LANGUAGE
    FlexibleContexts
  #-}

module Process.Client where

import Process.Types
import Config
import System.Process
import Application.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Monad.Catch


openSimpleWallet :: MonadApp m => m Process
openSimpleWallet = do -- FIXME: Bracket with `exit`
  client <- walletCliPath . walletConfig <$> get
  let p = proc client [] -- FIXME: Latest --daemon-host flag
                         -- FIXME: Wallet file location
  r <- liftIO $ createProcess p
         { std_in = CreatePipe
         , std_out = CreatePipe
         , std_err = CreatePipe
         }
  case r of
    (Just sIn, Just sOut, Just sErr, _) ->
      pure Process
             { processName = client
             , stdinHandle = sIn
             , stdoutHandle = sOut
             , stderrHandle = sErr
             }
    _ -> throwM $ NotEnoughHandles client
