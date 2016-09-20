{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  , TemplateHaskell
  #-}

module Routes where

import Application.Types
import Templates.Master
import Pages.NotFound

import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.FileEmbed (embedDir)


routes :: RouterT (MiddlewareT AppM) sec AppM ()
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
  match (l_ "themes" </> l_ "default" </> l_ "assets" </> l_ "fonts" </> word </> o_)
    respondIcon

word :: EitherUrlChunk ('Just T.Text)
word = pred_ "word" Just


respondIcon :: T.Text -> MiddlewareT AppM
respondIcon f app req resp =
  case lookup (T.unpack f) icons of
     Nothing -> (action notFoundHandle) app req resp
     Just i  -> resp . responseLBS status200 [] $ LBS.fromStrict i


icons :: [(FilePath, BS.ByteString)]
icons = $(embedDir "./static/vendor/semantic/dist/themes/default/assets/fonts/")

homeHandle :: ActionT AppM ()
homeHandle = get $ html (Just AppWallets) ""

notFoundHandle :: ActionT AppM ()
notFoundHandle = get $ do
  htmlLight status404 notFoundContent
  text "404 :("
