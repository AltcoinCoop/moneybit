{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Main where

import Main.Options
-- import Config
import Application
import Application.Types

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import Network.Wai.Trans
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Web.Routes.Nested

import System.Directory
import System.Remote.Monitoring

import Data.Url
import Data.Maybe
import Data.Default
import Data.Monoid
import Control.Monad.Reader




main :: IO ()
main = do
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
  entry (fromJust $ port cliOpts) env undefined -- (mkMutable cfg)



-- | Entry point, post options parsing
entry :: Int -> Env -> Mutable -> IO ()
entry p env mut = do
  forkServer "localhost" (p-1)
  run p $
      gzip def
    . logStdoutDev
    . application
    $ failApp
  where
    application = runMiddlewareT (runAppM env mut) $
        contentMiddleware
      . securityMiddleware
    failApp _ respond =
      respond $ textOnly "404!" status404 []
   -- otherLogger app req resp = do
   --   print $ pathInfo req
   --   app req resp
