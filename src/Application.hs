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

import Control.Monad.Catch

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



defApp :: Monad m => ApplicationT m
defApp _ resp = resp $ textOnly "404" status404 []



app :: ApplicationT AppM
app =
  let a = securityMiddleware
        . contentMiddleware
        $ defApp
  in  a `catchApplicationT` catchApiException
        `catchApplicationT` catchAuthException
