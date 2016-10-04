{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , QuasiQuotes
  #-}

module Templates.Master where

import Application.Types

import Data.Url
import Data.Aeson ((.=), encode, object, Value, toJSON)
import Web.Page.Lucid
import Web.Routes.Nested hiding (get)
import qualified Network.Wai.Middleware.ContentType.Types as CT
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Data.ByteString as BS
import Network.HTTP.Types
import Lucid
import Text.Heredoc (there, here)
import Path.Extended

import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Default
import Data.Markup as M
import Control.Monad.Trans
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State (modify, get)


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



modulePost :: LT.Text
modulePost = [here|if (window.module) module = window.module;|]



masterPage :: MonadApp m => WebPage (HtmlT m ()) T.Text
masterPage =
  let page :: MonadApp m => WebPage (HtmlT m ()) T.Text
      page = def
  in  page { pageTitle = "Moneybit Monero Wallet"
           , metaVars = do meta_ [charset_ "utf-8"]
                           meta_ [httpEquiv_ "X-UA-Compatible", content_ "IE=edge,chrome=1"]
                           meta_ [name_ "viewport", content_ "width-device-width, initial-scale=1.0, maximum-scale=1.0"]
           , styles = do
               host <- envAuthority <$> lift ask
               hoist (`runAbsoluteUrlT` host) $ do
                  semanticCss <- lift (toLocation SemanticCss)
                  deploy M.Css Remote semanticCss
                  cryptocoins <- lift (toLocation CryptoCoinsCss)
                  deploy M.Css Remote cryptocoins
                  cryptocoinsColors <- lift (toLocation CryptoCoinsColorsCss)
                  deploy M.Css Remote cryptocoinsColors
               inlineStyles
           , bodyScripts = do
               host <- envAuthority <$> lift ask
               deploy M.JavaScript Inline modulePre
               hoist (`runAbsoluteUrlT` host) $ do
                  jquery <- lift (toLocation JQuery)
                  deploy M.JavaScript Remote jquery
                  qrious <- lift (toLocation Qrious)
                  deploy M.JavaScript Remote qrious
                  semanticJs <- lift (toLocation SemanticJs)
                  deploy M.JavaScript Remote semanticJs
                  clipboardJs <- lift (toLocation ClipboardJs)
                  deploy M.JavaScript Remote clipboardJs
                  scryptJs <- lift (toLocation ScryptJs)
                  deploy M.JavaScript Remote scryptJs
                  naclJs <- lift (toLocation NaClJs)
                  deploy M.JavaScript Remote naclJs
                  zxcvbnJs <- lift (toLocation ZxcvbnJs)
                  deploy M.JavaScript Remote zxcvbnJs
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

      ws <- configWallets . config <$> lift get
      let ws' = LT.decodeUtf8 $ encode $ object $ (\w -> "name" .= w) <$> ws
          flags :: LT.Text
          flags = LT.unwords
            [ [here|
var host_ = "http://localhost:3000";
                      |]
            , "var flags_ = " <> ws' <> ";"
            ]
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
