{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  , RecordWildCards
  , NamedFieldPuns
  #-}

module Routes where

import Routes.Assets
import Api
import Application.Types
import Templates.Master
import Pages.NotFound
import Monero.Wallet.Process as Monero
import Monero.Wallet.RPC     as MoneroRPC
import Monero.Types          as Monero

import Data.Aeson as A hiding (json)
import Data.Time
import Data.STRef
import Data.Strict.Tuple
import Data.Monoid
import qualified Data.Map.Strict as Map
import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Base58 as BS58
import qualified Data.ByteString.Lazy as LBS
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Logger
import qualified Control.Monad.State as S
import qualified System.FilePath as F
import Crypto.Random (getRandomBytes)
import Control.Concurrent (threadDelay) -- FIXME
import System.Directory (removeFile)



routes :: RouterT (MiddlewareT AppM) sec AppM ()
routes = do
  -- Casual Pages
  matchHere (action homeHandle)
  match (l_ "config" </> o_) (action homeHandle)
  match (l_ "wallets" </> o_) (action homeHandle)
  matchGroup (l_ "wallet" </> word </> o_) $ do
    matchHere (\w -> action homeHandle)
    match (l_ "open"         </> o_) openHandle

    match (l_ "overview"     </> o_) (\w -> action homeHandle)
    match (l_ "transactions" </> o_) (\w -> action homeHandle)

    match (l_ "send"         </> o_) sendHandle
    match (l_ "receive"      </> o_) (\w -> action homeHandle)
    match (l_ "integrated"   </> o_) integratedHandle
    match (l_ "history"      </> o_) historyHandle
    match (l_ "seeds"        </> o_) seedsHandle

    match (l_ "close"        </> o_) closeHandle
    match (l_ "delete"       </> o_) deleteHandle
  match (l_ "new"     </> o_) newHandle
  match (l_ "recover" </> o_) recoverHandle

  -- Not Found
  match (l_ "not-found" </> o_) (action homeHandle) -- :v
  matchAny notFoundHandle

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
openHandle w app req resp = do
  logInfoN $ "Opening wallet: " <> w
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req -- FIXME account for chunking
        case A.decode b of
          Nothing -> throwM $ OpenDecodeError b
          Just OpenRequest{..} -> do
            -- TODO: Check session password

            wRef <- envOpenWallets <$> ask
            mW <- liftIO $ stToIO $ do
                    wallets <- readSTRef wRef
                    pure $ Map.lookup w wallets

            cfg <- case mW of
              Just (cfg' :!: _) -> pure cfg'
              Nothing -> do
                pConf <- lift makeWalletProcessConfig
                let oConf :: OpenWalletConfig
                    oConf = OpenWalletConfig
                              { openWalletName     = w
                              , openWalletPassword = openPassword
                              }

                liftIO $ do
                  (cfg',hs') <- openWallet pConf oConf
                  stToIO $ modifySTRef wRef (Map.insert w (cfg' :!: hs'))
                  pure cfg'

            r <- liftIO $ do
              Monero.Balance
                { balance         = b
                , unlockedBalance = ub
                } <- getBalance cfg
              MoneroRPC.GotAddress (Monero.Address (Monero.Base58String a))
                <- MoneroRPC.getAddress cfg
              pure OpenResponse
                { openBalance = Api.Balance { balanceBalance  = fromIntegral b  / 1e12
                                            , balanceUnlocked = fromIntegral ub / 1e12
                                            }
                , openAddress = a
                , openHistory = []
                , openHistoryMore = True
                }

            json r
  mid app req resp

closeHandle :: T.Text -> MiddlewareT AppM
closeHandle w = action $ get $ do
  lift $ do
    Env{envOpenWallets} <- ask
    liftIO $ do
      mW <- stToIO $ Map.lookup w <$> readSTRef envOpenWallets
      case mW of
        Nothing -> throwM $ WalletNotOpen w
        Just (_ :!: hs) -> do
          closeWallet hs
          stToIO $ modifySTRef envOpenWallets $ Map.delete w
  text "closed"


deleteHandle :: T.Text -> MiddlewareT AppM
deleteHandle w = action $ get $ do
  -- FIXME: Authenticate
  lift $ do
    Env{envOpenWallets} <- ask
    liftIO $ do
      mW <- stToIO $ Map.lookup w <$> readSTRef envOpenWallets
      case mW of
        Nothing -> throwM $ WalletNotOpen w
        Just (_ :!: hs) -> do
          closeWallet hs
          stToIO $ modifySTRef envOpenWallets $ Map.delete w
    Config{configWalletsPath} <- config <$> S.get
    liftIO $ mapM_ (\f -> removeFile $ configWalletsPath F.</> f)
               [ T.unpack w
               , T.unpack w ++ ".keys"
               , T.unpack w ++ ".address.txt"
               , T.unpack w ++ ".log"
               ]
  text "deleted"


integratedHandle :: T.Text -> MiddlewareT AppM
integratedHandle w = action $ get $ do
  integrated <- lift $ do
    Env{envOpenWallets} <- ask
    liftIO $ do
      mW <- stToIO $ Map.lookup w <$> readSTRef envOpenWallets
      case mW of
        Nothing -> throwM $ WalletNotOpen w
        Just (cfg :!: _) -> do
          MadeIntegratedAddress (Address (Base58String i))
            <- makeIntegratedAddress cfg $ MakeIntegratedAddress Nothing
          pure i
  json integrated -- FIXME include 64bit payment id?



seedsHandle :: T.Text -> MiddlewareT AppM
seedsHandle w = action $ get $ do
  wRef <- envOpenWallets <$> ask
  mW   <- liftIO $ stToIO $ Map.lookup w <$> readSTRef wRef
  (mnemonic,viewkey) <- case mW of
    Nothing          -> throwM $ WalletNotOpen w
    Just (cfg :!: _) -> liftIO $ do
      (MoneroRPC.QueriedKey m)
        <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyMnemonic
      (MoneroRPC.QueriedKey v)
        <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyView
      pure (m,v)
  json SeedsResponse
    { seedsMnemonic = mnemonic
    , seedsViewkey  = viewkey
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
              json HistoryResponse -- FIXME
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
            Nothing              -> throwM $ SendDecodeError b
            Just SendRequest{..} -> do
              json ("D:" :: T.Text)
  in  mid app req resp



-- Creation

newHandle :: MiddlewareT AppM
newHandle app req resp = do
  logInfoN "Creating wallet"
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req
        case A.decode b of
          Nothing -> throwM $ NewDecodeError b
          Just NewRequest{..} -> do
            pConf <- lift makeWalletProcessConfig
            let mConf :: MakeWalletConfig
                mConf = MakeWalletConfig
                          { makeWalletName     = newName
                          , makeWalletPassword = newPassword
                          , makeWalletLanguage = toMoneroLanguage newLanguage
                          }
            mnemonic <- liftIO $ do
              putStrLn "Creating..."
              makeWallet pConf mConf
              threadDelay 1000000
              putStrLn "Opening..."
              (cfg,hs) <- openWallet pConf $ OpenWalletConfig newName newPassword
              threadDelay 1000000
              putStrLn "Getting Seed..."
              (MoneroRPC.QueriedKey mn)
                <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyMnemonic
              putStrLn "Closing..."
              closeWallet hs
              pure mn
            lift $ configure $ \c@Config{configWallets} ->
              c { configWallets = T.unpack newName : configWallets }
            json mnemonic
  mid app req resp


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



notFoundHandle :: MiddlewareT AppM
notFoundHandle = action $ get $ do
  htmlLight status404 notFoundContent
