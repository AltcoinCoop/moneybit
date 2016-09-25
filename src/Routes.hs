{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  , TemplateHaskell
  , QuasiQuotes
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
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as BS58
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Control.Arrow (second)
import Control.Monad (forM_)
import Data.FileEmbed (embedDir)
import Crypto.Random (getRandomBytes)
import Text.Heredoc (there, here)

import Debug.Trace


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

  -- Assets
  matchGroup (l_ "static" </> o_) $ do
    matchOn JavaScript "jquery.min" $ LT.encodeUtf8 jquery
    matchOn JavaScript "qrious" $ LT.encodeUtf8 qriousJs
    matchGroup (l_ "cryptocoins" </> o_) $ do
      matchGroup (l_ "SVG" </> o_) $
        forM_ cryptocoinsSvgs $ \(f,b) ->
          let (f',_) = T.breakOnEnd "." $ T.pack f
          in  matchOn (Other "svg") (T.dropEnd 1 f') $ LBS.fromStrict b
      match (l_ "fonts" </> l_ "cryptocoins" </> o_) $ action $ get $
        forM_ cryptocoinsIcons $ \(f,b) ->
          let (_,e) = T.breakOnEnd "." $ T.pack f
          in  bytestring (Other e) $ LBS.fromStrict b
      matchOn Css "cryptocoins" $ LT.encodeUtf8 cryptocoins
      matchOn Css "cryptocoins-colors" $ LT.encodeUtf8 cryptocoinsColors
    matchGroup (l_ "semantic" </> o_) $ do
      match (l_ "semantic" </> o_) $ action $ get $ do
        bytestring Css        $ LT.encodeUtf8 semanticCss
        bytestring JavaScript $ LT.encodeUtf8 semanticJs
      match (l_ "themes" </> l_ "default" </> l_ "assets" </> l_ "fonts" </> l_ "icons" </> o_) $ action $ get $
        forM_ icons $ \(f,b) ->
          let (_,e) = T.breakOnEnd "." $ T.pack f
          in  bytestring (Other e) $ LBS.fromStrict b

  matchAny (action notFoundHandle)



word :: EitherUrlChunk ('Just T.Text)
word = pred_ "word" Just


matchOn :: FileExt -> T.Text -> LBS.ByteString -> RouterT (MiddlewareT AppM) sec AppM ()
matchOn e f = match (l_ f </> o_) . action . get . bytestring e


respond200LBS :: LBS.ByteString -> MiddlewareT AppM
respond200LBS b _ _ resp = resp $ response200LBS b
  where
    response200LBS :: LBS.ByteString -> Response
    response200LBS = responseLBS status200 []


-- Assets

icons :: [(FilePath, BS.ByteString)]
icons = $(embedDir "./frontend/bower_components/semantic/dist/themes/default/assets/fonts/")


semanticJs :: LT.Text
semanticJs = [there|./frontend/bower_components/semantic/dist/semantic.min.js|]

semanticCss :: LT.Text
semanticCss = [there|./frontend/bower_components/semantic/dist/semantic.min.css|]


qriousJs :: LT.Text
qriousJs = [there|./frontend/bower_components/qrious/dist/umd/qrious.min.js|]


cryptocoins :: LT.Text
cryptocoins = [there|./frontend/deps/cryptocoins/cryptocoins.css|]

cryptocoinsColors :: LT.Text
cryptocoinsColors = [there|./frontend/deps/cryptocoins/cryptocoins-colors.css|]

cryptocoinsIcons :: [(FilePath, BS.ByteString)]
cryptocoinsIcons = $(embedDir "./frontend/deps/cryptocoins/fonts/")

cryptocoinsSvgs :: [(FilePath, BS.ByteString)]
cryptocoinsSvgs = $(embedDir "./frontend/deps/cryptocoins/SVG/")


jquery :: LT.Text
jquery = [there|./frontend/bower_components/jquery/dist/jquery.min.js|]


-- Handles

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

