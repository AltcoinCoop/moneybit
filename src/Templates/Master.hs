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


masterPage :: MonadApp m => WebPage (HtmlT m ()) T.Text
masterPage =
  let page :: MonadApp m => WebPage (HtmlT m ()) T.Text
      page = def
  in  page { pageTitle = "Moneybit Monero Wallet"
           , metaVars = do meta_ [charset_ "utf-8"]
                           meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
                           meta_ [name_ "viewport", content_ "width-device-width, initial-scale=1.0, maximum-scale=1.0"]
           , styles = do
               deploy M.Css Remote ("https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.1.8/semantic.min.css" :: T.Text)
               deploy M.Css Remote ("https://cdnjs.cloudflare.com/ajax/libs/font-mfizz/2.3.0/font-mfizz.css" :: T.Text)
               inlineStyles
           , bodyScripts =
               deploy M.JavaScript Remote ("https://cdnjs.cloudflare.com/ajax/libs/jquery/3.0.0-beta1/jquery.min.js" :: T.Text)
               deploy M.JavaScript Remote ("https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.1.8/semantic.min.js" :: T.Text)
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
          flags :: LT.Text
          flags =[here| var host_ = "http://localhost:3000";
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
          init :: LT.Text
          init = [there|./frontend/init.js|]
      deploy M.JavaScript Inline elm -- FIXME
      deploy M.JavaScript Inline flags
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
