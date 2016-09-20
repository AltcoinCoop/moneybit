{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  #-}

module Routes where

import Application.Types
import Templates.Master
import Pages.NotFound

import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import qualified Data.Text as T


routes :: ( MonadApp m
          ) => RouterT (MiddlewareT m) sec m ()
routes = do
  matchHere (action homeHandle)
  match (l_ "config" </> o_) (action homeHandle)
  match (l_ "wallets" </> o_) (action homeHandle)
  matchGroup (l_ "wallet" </> word </> o_) $ do
    match (l_ "overview"     </> o_) (\w -> action homeHandle)
    match (l_ "send"         </> o_) (\w -> action homeHandle)
    match (l_ "receive"      </> o_) (\w -> action homeHandle)
    match (l_ "transactions" </> o_) (\w -> action homeHandle)
  matchAny (action notFoundHandle)
  where
    homeHandle = get $ html (Just AppWallets) ""
    notFoundHandle = get $ do
      htmlLight status404 notFoundContent
      text "404 :("


word :: EitherUrlChunk ('Just T.Text)
word = pred_ "word" Just
