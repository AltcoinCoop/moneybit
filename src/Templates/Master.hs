{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , QuasiQuotes
  #-}

module Templates.Master where

import Application.Types

import Data.Url
import Web.Page.Lucid
import Web.Routes.Nested
import qualified Network.Wai.Middleware.ContentType.Types as CT
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString as BS
import Network.HTTP.Types
import Lucid
import Text.Heredoc (there, here)

import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Default
import Data.Markup as M
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State (modify)


-- | Render without @mainTemplate@
htmlLight :: ( MonadApp m
             ) => Status
               -> HtmlT (AbsoluteUrlT m) a
               -> FileExtListenerT m ()
htmlLight s content = do
  hostname <- envAuthority <$> lift ask
  bs       <- lift $ runAbsoluteUrlT (renderBST content) hostname

  bytestring CT.Html bs
  modify . HM.map $ mapStatus (const s)
                  . mapHeaders ([("content-Type","text/html")] ++)


-- | Shortcut for rendering with a template
html :: ( MonadApp m
        ) => Maybe AppLinks
          -> HtmlT (AbsoluteUrlT m) ()
          -> FileExtListenerT m ()
html mLink content = htmlLight status200 $ mainTemplate mLink content



modulePre :: LT.Text
modulePre = [here|if (typeof module === 'object') {window.module = module; module = undefined;}|]


jquery :: LT.Text
jquery = [there|./frontend/bower_components/jquery/dist/jquery.min.js|]


semanticJs :: LT.Text
semanticJs = [there|./frontend/bower_components/semantic/dist/semantic.min.js|]


modulePost :: LT.Text
modulePost = [here|if (window.module) module = window.module;|]


semanticCss :: LT.Text
semanticCss = [there|./frontend/bower_components/semantic/dist/semantic.min.css|]


clipboardJs :: LT.Text
clipboardJs = [there|./frontend/bower_components/clipboard/dist/clipboard.min.js|]


qriousJs :: LT.Text
qriousJs = [there|./frontend/bower_components/qrious/dist/umd/qrious.min.js|]


cryptocoins :: LT.Text
cryptocoins = [there|./frontend/deps/cryptocoins/cryptocoins.css|]

cryptocoinsColors :: LT.Text
cryptocoinsColors = [there|./frontend/deps/cryptocoins/cryptocoins-colors.css|]


scryptJs :: LT.Text
scryptJs = [there|./frontend/bower_components/js-scrypt/browser/scrypt.js|]


naclJs :: LT.Text
naclJs = [there|./frontend/bower_components/js-nacl/lib/nacl_factory.js|]


masterPage :: MonadApp m => WebPage (HtmlT m ()) T.Text
masterPage =
  let page :: MonadApp m => WebPage (HtmlT m ()) T.Text
      page = def
  in  page { pageTitle = "Moneybit Monero Wallet"
           , metaVars = do meta_ [charset_ "utf-8"]
                           meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
                           meta_ [name_ "viewport", content_ "width-device-width, initial-scale=1.0, maximum-scale=1.0"]
           , styles = do
               deploy M.Css Inline semanticCss
               deploy M.Css Inline cryptocoins
               deploy M.Css Inline cryptocoinsColors
               inlineStyles
           , bodyScripts = do
               deploy M.JavaScript Inline modulePre
               deploy M.JavaScript Inline jquery
               deploy M.JavaScript Inline qriousJs
               deploy M.JavaScript Inline semanticJs
               deploy M.JavaScript Inline clipboardJs
               deploy M.JavaScript Inline scryptJs
               deploy M.JavaScript Inline naclJs
               inlineScripts
           }
  where
    inlineStyles :: MonadApp m => HtmlT m ()
    inlineStyles = do
      let css :: LT.Text
          css = [there|./frontend/style.css|]
      deploy M.Css Inline css

    inlineScripts :: MonadApp m => HtmlT m ()
    inlineScripts = do
      let elm :: LT.Text
          elm = [there|./frontend/dist/Main.min.js|]
      deploy M.JavaScript Inline elm
      deploy M.JavaScript Inline modulePost
      let flags :: LT.Text
          flags =[here|
var host_ = "http://localhost:3000";
var flags_ = {
  "wallets" : [
    {
      "name" : "foo"
    },
    {
      "name" : "bar"
    }
  ]
};
                      |]
      deploy M.JavaScript Inline flags
      let init :: LT.Text
          init = [there|./frontend/init.js|]
      deploy M.JavaScript Inline init


masterTemplate :: ( MonadApp m
                  ) => Maybe AppLinks
                    -> WebPage (HtmlT (AbsoluteUrlT m) ()) T.Text
                    -> HtmlT (AbsoluteUrlT m) ()
                    -> HtmlT (AbsoluteUrlT m) ()
masterTemplate _ = template


mainTemplate :: ( MonadApp m
                ) => Maybe AppLinks
                  -> HtmlT (AbsoluteUrlT m) ()
                  -> HtmlT (AbsoluteUrlT m) ()
mainTemplate state =
  masterTemplate state masterPage


appendTitle :: WebPage (HtmlT m ()) T.Text
            -> T.Text
            -> WebPage (HtmlT m ()) T.Text
appendTitle page x = page { pageTitle = x <> " « " <> pageTitle page }
