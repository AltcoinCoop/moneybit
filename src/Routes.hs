{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  , RecordWildCards
  #-}

module Routes where

import Routes.Assets
import Api
import Application.Types
import Templates.Master
import Pages.NotFound

import Data.Aeson as A hiding (json)
import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Base58 as BS58
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Control.Arrow (second)
import Control.Monad (forM_)
import Control.Monad.Catch
import Crypto.Random (getRandomBytes)
import Control.Concurrent (threadDelay) -- FIXME

import Data.Time



routes :: RouterT (MiddlewareT AppM) sec AppM ()
routes = do
  -- Casual Pages
  matchHere (action homeHandle)
  match (l_ "config" </> o_) (action homeHandle)
  match (l_ "wallets" </> o_) (action homeHandle)
  matchGroup (l_ "wallet" </> word </> o_) $ do
    matchHere (\w -> action homeHandle)
    match (l_ "overview"     </> o_) (\w -> action homeHandle)
    match (l_ "send"         </> o_) sendHandle
    match (l_ "receive"      </> o_) (\w -> action homeHandle)
    match (l_ "transactions" </> o_) (\w -> action homeHandle)
    match (l_ "seeds"        </> o_) (\w -> action homeHandle)
    match (l_ "open"         </> o_) openHandle
    match (l_ "close"        </> o_) closeHandle
    match (l_ "integrated"   </> o_) integratedHandle
    match (l_ "history"      </> o_) historyHandle
    match (l_ "seeds"        </> o_) seedsHandle
  match (l_ "new"     </> o_) newHandle
  match (l_ "recover" </> o_) recoverHandle

  -- Not Found
  match (l_ "not-found" </> o_) (action homeHandle) -- :v
  matchAny (action notFoundHandle)

  -- Utils
  match (l_ "newPaymentId" </> o_) newPaymentIdHandle
  match (l_ "transcode" </> o_)    transcodeHandle

  imageRoutes
  staticRoutes




word :: EitherUrlChunk ('Just T.Text)
word = pred_ "word" Just



respond200LBS :: LBS.ByteString -> MiddlewareT AppM
respond200LBS b _ _ resp = resp $ response200LBS b
  where
    response200LBS :: LBS.ByteString -> Response
    response200LBS = responseLBS status200 []


-- Assets

-- Handles

homeHandle :: ActionT AppM ()
homeHandle = get $ html (Just AppWallets) ""


newPaymentIdHandle :: MiddlewareT AppM
newPaymentIdHandle = action $ get $ do
  xs <- liftIO $ getRandomBytes 32
  json $ T.decodeUtf8 $ BS16.encode xs



transcodeHandle :: MiddlewareT AppM
transcodeHandle app req resp =
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req
        case A.decode b of
          Nothing -> throwM $ TranscodeDecodeError b
          Just TranscodeRequest{..} -> do
            i <- case transcodeFrom of
                   Base16 -> case BS16.decode transcodeInput of
                     (x,e) | e == BS.empty -> pure x
                     _ -> throwM $ TranscodeDecodeByteError transcodeInput
                   Base64 -> case BS64.decode transcodeInput of
                     Right x -> pure x
                     _ -> throwM $ TranscodeDecodeByteError transcodeInput
                   Base58 -> case BS58.decodeBase58 BS58.bitcoinAlphabet transcodeInput of
                     Nothing -> throwM $ TranscodeDecodeByteError transcodeInput
                     Just x  -> pure x
            let o = case transcodeTo of
                  Base16 -> BS16.encode i
                  Base64 -> BS64.encode i
                  Base58 -> BS58.encodeBase58 BS58.bitcoinAlphabet i
            json $ T.decodeUtf8 o
  in  mid app req resp




-- * API

openHandle :: T.Text -> MiddlewareT AppM
openHandle w app req resp =
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req -- FIXME account for chunking
        case A.decode b of
          Nothing -> throwM $ OpenDecodeError b
          Just OpenRequest{..} -> do
            liftIO $ threadDelay 1000000
            json OpenResponse
              { openBalance = Balance { balanceBalance = 5
                                      , balanceUnlocked = 5
                                      }
              , openAddress = "asdf"
              , openHistory = []
              , openHistoryMore = True
              } -- FIXME needs genuine content
  in  mid app req resp

closeHandle :: T.Text -> MiddlewareT AppM
closeHandle w = action $ get $ text "closed" -- FIXME actually close


integratedHandle :: T.Text -> MiddlewareT AppM
integratedHandle w = action $ get $ json
  ("asdf" :: T.Text) -- FIXME include 64bit payment id?



seedsHandle :: T.Text -> MiddlewareT AppM
seedsHandle w = action $ get $ json
  SeedsResponse
    { seedsMnemonic = "asdf"
    , seedsViewkey  = "asdf"
    , seedsSpendkey = "asdf"
    }


historyHandle :: T.Text -> MiddlewareT AppM
historyHandle w app req resp =
  let mid = action $ do
        get $ json HistoryResponse
                { historyHistory =
                    [ Transaction
                        { transactionValue = 6
                        , transactionTxId = "asdf"
                        , transactionConfirmations = 4
                        , transactionDate = UTCTime (ModifiedJulianDay 0) 0
                        }
                    ]
                , historyMore = True
                } -- FIXME
        post $ do
          b <- liftIO $ strictRequestBody req
          case A.decode b of
            Nothing -> throwM $ HistoryDecodeError b
            Just HistoryRequest{..} -> do
              liftIO $ threadDelay 1000000
              json HistoryResponse
                { historyHistory = []
                , historyMore = True
                } -- FIXME
  in  mid app req resp



sendHandle :: T.Text -> MiddlewareT AppM
sendHandle w app req resp =
  let mid = action $ do
        homeHandle
        post $ do
          b <- liftIO $ strictRequestBody req
          case A.decode b of
            Nothing -> throwM $ SendDecodeError b
            Just SendRequest{..} -> do
              liftIO $ threadDelay 1000000
              json ("asdf" :: T.Text) -- FIXME transaction Ids? addresses et. al?
  in  mid app req resp



-- Creation

newHandle :: MiddlewareT AppM
newHandle app req resp =
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req
        case A.decode b of
          Nothing -> throwM $ NewDecodeError b
          Just NewRequest{..} -> do
            liftIO $ threadDelay 1000000
            text "Ok!" -- FIXME
  in  mid app req resp


recoverHandle :: MiddlewareT AppM
recoverHandle app req resp =
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req
        case A.decode b of
          Nothing -> throwM $ RecoverDecodeError b
          Just RecoverRequest{..} -> do
            liftIO $ threadDelay 1000000
            text "Ok!" -- FIXME
  in  mid app req resp



notFoundHandle :: ActionT AppM ()
notFoundHandle = get $ do
  htmlLight status404 notFoundContent
  text "404 :("
