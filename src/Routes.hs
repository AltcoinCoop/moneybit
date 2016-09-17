{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Routes where

import Application.Types
import Templates.Master
import Pages.NotFound

import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types


routes :: ( MonadApp m
          ) => RouterT (MiddlewareT m) sec m ()
routes = do
  matchHere (action homeHandle)
  matchAny (action notFoundHandle)
  where
    homeHandle = get $ html (Just AppHome) "home"
    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("

