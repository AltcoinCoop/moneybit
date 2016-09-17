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


-- TODO Codify:
{-

* Every wallet has a wallet file = ~/.moneybit/?
* Each wallet has a password - creating one should have passwords
  pre-inquired?
  - opening one should be done with --password

# maintain as much as possible in CLI args - piping is a bizzotch

-}


openSimpleWallet :: MonadApp m => m Process
openSimpleWallet = do -- FIXME: Bracket with `exit`
  cfg <- get
  let wallet = walletConfig cfg
      client = walletCliPath wallet
      monerod = walletMoneroDNode wallet

      args = case monerod of
        LocalNode -> []
        RemoteNode r -> ["--daemon-host", r]

      p = proc client args -- FIXME: Wallet file location
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
