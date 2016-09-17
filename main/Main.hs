{-# LANGUAGE
    DeriveGeneric
  , ScopedTypeVariables
  , OverloadedStrings
  #-}

module Main where

import Main.Options
import Application
import Application.Types

import           Options.Applicative
import qualified Data.Yaml as Y
import qualified Data.Aeson.Types as A
import Network.Wai.Trans
import Network.Wai.Middleware.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Network.HTTP.Types
import Web.Routes.Nested

import System.Directory

import Data.Url
import Data.Maybe
import Data.Default
import Data.Monoid
import Control.Monad.Reader




main :: IO ()
main = do
  -- CLI Opts
  let opts :: ParserInfo App
      opts = info (helper <*> app)
        ( fullDesc
       <> progDesc "Serve application from PORT over HOST"
       <> header "middleman - a web server" )

  (commandOpts :: App) <- execParser opts

  -- Yaml Opts
  let yamlConfigPath = fromMaybe
        "config/config.yaml" $
        configPath commandOpts

  -- Yaml bug
  yamlConfigExists   <- doesFileExist yamlConfigPath
  yamlConfigContents <-
    if yamlConfigExists
    then readFile yamlConfigPath
    else pure ""

  mYamlConfig <-
    if yamlConfigExists && yamlConfigContents /= ""
    then Y.decodeFile yamlConfigPath
    else pure Nothing

  let yamlConfig :: AppOpts
      yamlConfig = fromMaybe def mYamlConfig

      config :: AppOpts
      config = def <> yamlConfig <> options commandOpts

  entry (fromJust $ port config) =<< appOptsToEnv config



-- | Entry point, post options parsing
entry :: Int -> Env -> IO ()
entry p env =
  run p $
      staticPolicy (noDots >-> addBase "static")
    . gzip def
    . logStdoutDev
    . application
    $ failApp
  where
    application = runMiddlewareT (runAppM env) $
        contentMiddleware
      . securityMiddleware
      . staticMiddleware
    failApp _ respond =
      respond $ textOnly "404!" status404 []
