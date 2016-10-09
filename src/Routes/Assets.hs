{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  , OverloadedStrings
  #-}

module Routes.Assets where

import Application.Types

import Text.Heredoc (there)
import Data.FileEmbed (embedFile, embedDir)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Web.Routes.Nested
import Network.Wai.Trans
import Control.Monad (forM_)



staticRoutes :: RouterT (MiddlewareT AppM) sec AppM ()
staticRoutes = matchGroup (l_ "static" </> o_) $ do
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
  matchGroup (l_ "semantic" </> o_) $ do
    match (l_ "semantic" </> o_) $ action $ get $ do
      bytestring Css        $ LT.encodeUtf8 semanticCss
      bytestring JavaScript $ LT.encodeUtf8 semanticJs
    match (l_ "themes" </> l_ "default" </> l_ "assets" </> l_ "fonts" </> l_ "icons" </> o_) $ action $ get $
      forM_ icons $ \(f,b) ->
        let (_,e) = T.breakOnEnd "." $ T.pack f
        in  bytestring (Other e) $ LBS.fromStrict b
  matchOn JavaScript "clipboard" $ LT.encodeUtf8 clipboardJs
  matchOn JavaScript "scrypt"    $ LT.encodeUtf8 scryptJs
  matchOn JavaScript "nacl"      $ LT.encodeUtf8 naclJs
  matchOn JavaScript "zxcvbn"    $ LT.encodeUtf8 zxcvbnJs


imageRoutes :: RouterT (MiddlewareT AppM) sec AppM ()
imageRoutes =
  matchGroup (l_ "images" </> o_) $
    matchOn (Other "png") "seeds_stripe" $ LBS.fromStrict seedsStripePng


matchOn :: FileExt -> T.Text -> LBS.ByteString -> RouterT (MiddlewareT AppM) sec AppM ()
matchOn e f = match (l_ f </> o_) . action . get . bytestring e


-- * Assets


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

cryptocoinsIcons :: [(FilePath, BS.ByteString)]
cryptocoinsIcons = $(embedDir "./frontend/deps/cryptocoins/fonts/")

cryptocoinsSvgs :: [(FilePath, BS.ByteString)]
cryptocoinsSvgs = $(embedDir "./frontend/deps/cryptocoins/SVG/")


jquery :: LT.Text
jquery = [there|./frontend/bower_components/jquery/dist/jquery.min.js|]

clipboardJs :: LT.Text
clipboardJs = [there|./frontend/bower_components/clipboard/dist/clipboard.min.js|]

scryptJs :: LT.Text
scryptJs = [there|./frontend/bower_components/js-scrypt/browser/scrypt.js|]

naclJs :: LT.Text
naclJs = [there|./frontend/bower_components/js-nacl/lib/nacl_factory.js|]

zxcvbnJs :: LT.Text
zxcvbnJs = [there|./frontend/bower_components/zxcvbn/dist/zxcvbn.js|]


seedsStripePng :: BS.ByteString
seedsStripePng = $(embedFile "./frontend/images/seeds_stripe.png")

