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


securityMiddleware :: MiddlewareT AppM
securityMiddleware app req resp = do
  extractAuth authorize req routes
  app req resp


contentMiddleware :: MiddlewareT AppM
contentMiddleware =
  route routes
