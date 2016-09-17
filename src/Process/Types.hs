module Process.Types where

import GHC.IO.Handle (Handle)


data Process = Process
  { processName  :: String
  , stdinHandle  :: Handle
  , stdoutHandle :: Handle
  , stderrHandle :: Handle
  } deriving (Show, Eq)
