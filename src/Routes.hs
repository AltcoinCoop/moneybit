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
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as BS58
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Data.FileEmbed (embedDir)
import Crypto.Random (getRandomBytes)


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
  match (l_ "newPaymentId" </> o_) (action newPaymentIdHandle)
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
icons = $(embedDir "./frontend/bower_components/semantic/dist/themes/default/assets/fonts/")

homeHandle :: ActionT AppM ()
homeHandle = get $ html (Just AppWallets) ""


newPaymentIdHandle :: ActionT AppM ()
newPaymentIdHandle = get $ do
  xs <- liftIO $ getRandomBytes 32
  json $ T.decodeUtf8 $ BS58.encodeBase58 BS58.bitcoinAlphabet xs

notFoundHandle :: ActionT AppM ()
notFoundHandle = get $ do
  htmlLight status404 notFoundContent
  text "404 :("
