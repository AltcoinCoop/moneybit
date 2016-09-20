{-# LANGUAGE
    FlexibleContexts
  , DeriveGeneric
  , OverloadedStrings
  #-}

module Application where

import Application.Types
import Routes

import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types

import qualified Data.Text as T
import Control.Monad.Catch
import Control.Monad.Reader

import System.Directory
import GHC.Generics



data AuthRole = NeedsLogin

data AuthError = NeedsAuth
  deriving (Generic, Show)

instance Exception AuthError


authorize :: ( MonadThrow m
             ) => Request -> [AuthRole] -> m ()
authorize _ ss
  | null ss   = pure ()
  | otherwise = throwM NeedsAuth


securityMiddleware :: MonadApp m => MiddlewareT m
securityMiddleware app req resp = do
  extractAuth authorize req routes
  app req resp


contentMiddleware :: MonadApp m => MiddlewareT m
contentMiddleware =
  route routes


staticMiddleware :: MonadApp m => MiddlewareT m
staticMiddleware app req respond = do
    let fileRequested = T.unpack
                      . T.intercalate "/"
                      $ pathInfo req
    basePath <- envStatic <$> ask
    let file = basePath ++ "/" ++ fileRequested
    fileExists <- liftIO (doesFileExist file)
    if fileExists
    then respond $ responseFile status200 [] file Nothing
    else app req respond
