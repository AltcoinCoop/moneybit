module Process.Client where

import System.Process
import Application.Types

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class


openSimpleWallet :: MonadApp m => m Process
openSimpleWallet = do -- FIXME: Bracket with `exit`
  client <- envClient <$> ask
  r <- liftIO $ createProcess $
         proc client []
