{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  #-}

module Templates.Master where

import Application.Types

import Data.Url
import Web.Page.Lucid
import Web.Routes.Nested
import qualified Network.Wai.Middleware.ContentType.Types as CT
import qualified Data.Text as T
import Network.HTTP.Types
import Lucid

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
  in  page { pageTitle = "App"
           , styles =
               inlineStyles
           }
  where
    inlineStyles :: MonadApp m => HtmlT m ()
    inlineStyles =
      deploy M.Css Inline ("body {background: gray;}" :: T.Text)


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
