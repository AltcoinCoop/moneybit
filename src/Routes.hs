{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , FlexibleContexts
  , DataKinds
  , RecordWildCards
  , NamedFieldPuns
  #-}

module Routes where

import Routes.Assets (imageRoutes, staticRoutes)
import Api
import Api.WebSocket
import Api.WebSocket.RPC
import Application.Types
import Templates.Master
import Pages.NotFound
import Monero.Wallet.Process as Monero
import Monero.Wallet.RPC     as MoneroRPC
import Monero.Types          as Monero
import Data.Process          (ProcessHandles)
import Data.Json.RPC         (RPCConfig)

import Data.Aeson as A hiding (json)
import Data.Time
import Data.STRef
import Data.Strict.Tuple
import Data.Monoid
import qualified Data.Map.Strict as Map
import Web.Routes.Nested
import Network.Wai.Trans
import Network.HTTP.Types
import Network.WebSockets as WS hiding (Response)
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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait, link)
import System.Directory (removeFile)



routes :: RouterT (MiddlewareT AppM) sec AppM ()
routes = do
  -- Casual Pages
  matchHere (action homeHandle)
  match (l_ "config" </> o_) configHandle
  matchGroup (l_ "wallets" </> o_) $ do
    matchHere walletsHandle
    match (l_ "open" </> o_) openWalletsHandle
  match (l_ "new" </> o_) newHandle
  matchGroup (l_ "wallet" </> word </> o_) $ do
    matchHere (\w -> action homeHandle) -- FIXME: List open wallets?
    match (l_ "open"         </> o_) openHandle

    match (l_ "overview"     </> o_) (\w -> action homeHandle)
    match (l_ "transactions" </> o_) (\w -> action homeHandle)

    match (l_ "send"         </> o_) sendHandle
    match (l_ "receive"      </> o_) (\w -> action homeHandle)
    match (l_ "integrated"   </> o_) integratedHandle
    match (l_ "address"      </> o_) addressHandle
    match (l_ "balance"      </> o_) balanceHandle
    match (l_ "history"      </> o_) historyHandle -- TODO
    match (l_ "seeds"        </> o_) seedsHandle

    match (l_ "close"        </> o_) closeHandle
    match (l_ "delete"       </> o_) deleteHandle

  match (l_ "ws" </> o_) wsHandle

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


-- * Handles

-- ** Html

homeHandle :: ActionT AppM ()
homeHandle = get $ html (Just AppWallets) ""

notFoundHandle :: MiddlewareT AppM
notFoundHandle = action $ get $
  htmlLight status404 notFoundContent



-- ** API

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
                          , makeWalletSeed     = newMnemonic
                          }
            mnemonic <- liftIO $ do
              putStrLn "Creating..."
              makeWallet pConf mConf
              threadDelay 1000000
              putStrLn "Opening..."
              (cfg,hs) <- openWallet pConf $ OpenWalletConfig newName newPassword 1000000 (\r -> print r) -- FIXME: Websocket only?
              threadDelay 1000000
              putStrLn "Getting Seed..."
              (MoneroRPC.QueriedKey mn)
                <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyMnemonic
              putStrLn "Created."
              closeWallet hs
              pure mn
            lift $ configure $ \c@Config{configWallets} ->
              c { configWallets = T.unpack newName : configWallets }
            json mnemonic
  mid app req resp



openHandle :: T.Text -> MiddlewareT AppM
openHandle w app req resp = do
  logInfoN $ "Opening wallet: " <> w
  let mid = action $ post $ do
        b <- liftIO $ strictRequestBody req -- FIXME account for chunking
        case A.decode b of
          Nothing -> throwM $ OpenDecodeError b
          Just OpenRequest{..} -> do
            -- FIXME: Check session password

            Env{envOpenWallets} <- ask
            mW <- liftIO $ stToIO $ Map.lookup w <$> readSTRef envOpenWallets

            cfg <- case mW of
              Just (cfg' :!: _) -> pure cfg'
              Nothing -> do
                pConf <- lift makeWalletProcessConfig
                let oConf :: OpenWalletConfig
                    oConf = OpenWalletConfig
                              { openWalletName     = w
                              , openWalletPassword = openPassword
                              , openWalletInterval = 1000000
                              , openWalletProgress = print -- FIXME
                              }

                liftIO $ do
                  -- FIXME: Parse logfile and stop blocking on predicate, too
                  (cfg',hs') <- openWallet pConf oConf
                  stToIO $ modifySTRef envOpenWallets $
                    Map.insert w (cfg' :!: hs')
                  pure cfg'

            logInfoN $ "Found wallet: " <> w

            r <- liftIO $ do
              Monero.Balance
                { balance         = b
                , unlockedBalance = ub
                }                    <- getBalance cfg
              MoneroRPC.GotAddress a <- MoneroRPC.getAddress cfg
              pure OpenResponse
                { openBalance = Api.Balance
                    { balanceBalance  = fromIntegral b  / 1e12
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
    (_ :!: hs) <- findWallet w
    Env{envOpenWallets} <- ask
    liftIO $ do
      closeWallet hs
      stToIO $ modifySTRef envOpenWallets $ Map.delete w
  text "closed"


deleteHandle :: T.Text -> MiddlewareT AppM
deleteHandle w = action $ get $ do
  -- FIXME: Authenticate
  lift $ do
    (_ :!: hs) <- findWallet w
    Env{envOpenWallets} <- ask
    liftIO $ do
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


-- *** Wallet Components

integratedHandle :: T.Text -> MiddlewareT AppM
integratedHandle w app req resp =
  let mid = action $ do
              get $ do
                integrated <- lift $ do
                  (cfg :!: _) <- findWallet w
                  MadeIntegratedAddress (Address (Base58String i))
                    <- liftIO $ makeIntegratedAddress cfg $ MakeIntegratedAddress Nothing
                  pure i
                json integrated
              post $ do
                b <- liftIO $ strictRequestBody req
                xs <- case A.decode b of
                  Nothing -> throwM $ IntegratedDecodeError b
                  Just input@Base58String{} -> lift $ do
                    (cfg :!: _) <- findWallet w
                    GotSplitIntegratedAddress{..}
                      <- liftIO $ splitIntegratedAddress cfg $ SplitIntegratedAddress $
                            Address input
                    pure (gotSplitStandardAddress, gotSplitPaymentId)
                json xs
  in  mid app req resp



seedsHandle :: T.Text -> MiddlewareT AppM
seedsHandle w = action $ get $ do
  (mnemonic,viewkey) <- lift $ do
    (cfg :!: _) <- findWallet w
    liftIO $ do
      (MoneroRPC.QueriedKey m)
        <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyMnemonic
      (MoneroRPC.QueriedKey v)
        <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyView
      pure (m,v)
  json SeedsResponse
    { seedsMnemonic = mnemonic
    , seedsViewkey  = HexString viewkey
    , seedsSpendkey = HexString "asdf" -- FIXME
    }


addressHandle :: T.Text -> MiddlewareT AppM
addressHandle w = action $ get $ do
  GotAddress a <- lift $ do
    (cfg :!: _) <- findWallet w
    liftIO $ MoneroRPC.getAddress cfg
  json a


balanceHandle :: T.Text -> MiddlewareT AppM
balanceHandle w = action $ get $ do
  Monero.Balance{..} <- lift $ do
    (cfg :!: _) <- findWallet w
    liftIO $ getBalance cfg
  json ( fromMoneroAmount balance
       , fromMoneroAmount unlockedBalance
       )


historyHandle :: T.Text -> MiddlewareT AppM
historyHandle w app req resp =
  -- let mid = action $ do
  --       (cfg :!: _) <- lift $ findWallet w
  --       get $
  --         json HistoryResponse
  --               { historyHistory =
  --                   [ Transaction
  --                       { transactionValue = 6
  --                       , transactionTxId = HexString "asdf"
  --                       , transactionConfirmations = 4
  --                       , transactionDate = UTCTime (ModifiedJulianDay 0) 0
  --                       }
  --                   ]
  --               , historyMore = True
  --               } -- FIXME
  --       post $ do
  --         b <- liftIO $ strictRequestBody req
  --         case A.decode b of
  --           Nothing -> throwM $ HistoryDecodeError b
  --           Just HistoryRequest{..} -> do
  --             liftIO $ threadDelay 1000000
  --             json HistoryResponse -- FIXME
  --               { historyHistory = []
  --               , historyMore = True
  --               } -- FIXME
  -- in  mid app req resp
      resp $ textOnly "Not Implemented" status500 []



sendHandle :: T.Text -> MiddlewareT AppM
sendHandle w app req resp =
  let mid = action $ do
        homeHandle
        post $ do
          b <- liftIO $ strictRequestBody req
          case A.decode b of
            Nothing              -> throwM $ SendDecodeError b
            Just SendRequest{..} -> do
              lift $ do
                (cfg :!: _) <- findWallet w
                void $ liftIO $ transferSplit cfg MakeTransferSplit
                           { makeTransferSplitDestinations =
                               [ TransferDestination
                                   { destinationAmount  = toMoneroAmount sendAmount
                                   , destinationAddress = sendRecipient
                                   }
                               ]
                           , makeTransferSplitMixin = fromIntegral sendMixin
                           , makeTransferSplitUnlockTime = 0
                           , makeTransferSplitPaymentId = sendPaymentId
                           , makeTransferSplitGetTxKey     = False
                           , makeTransferSplitNewAlgorithm = True
                           }
              json ("Sent" :: T.Text)
  in  mid app req resp



-- ** Utils

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


walletsHandle :: MiddlewareT AppM
walletsHandle =
  action $ do
    homeHandle
    get $ do
      Config{configWallets} <- config <$> lift S.get
      json configWallets

openWalletsHandle :: MiddlewareT AppM
openWalletsHandle =
  action $ get $ do
    openWallets <- lift $ do
      Env{envOpenWallets} <- ask
      liftIO $ stToIO $ Map.keys <$> readSTRef envOpenWallets
    json openWallets


-- ** Utils

findWallet :: T.Text -> AppM (Pair RPCConfig ProcessHandles)
findWallet w = do
  Env{envOpenWallets} <- ask
  mW <- liftIO $ stToIO $ Map.lookup w <$> readSTRef envOpenWallets
  case mW of
    Nothing -> throwM $ WalletNotOpen w
    Just xs -> pure xs


-- * Config

configHandle :: MiddlewareT AppM
configHandle app req resp =
  let mid = action $ do
              get $ do
                cfg <- config <$> lift S.get
                json cfg
              post $ do
                b <- liftIO $ strictRequestBody req
                case A.decode b of
                  Nothing -> throwM $ ConfigDecodeError b
                  Just cfg@Config{} -> do
                    -- FIXME Authenticate
                    lift $ configure $ const cfg
                    json ("Configured" :: T.Text)
              homeHandle
  in  mid app req resp


-- * WebSockets

wsHandle :: MiddlewareT AppM
wsHandle app req resp = do
  env <- ask
  mut <- S.get
  let mid = websocketsOrT (runAppM env mut) defaultConnectionOptions $
            \pendingConn -> do
              conn <- liftIO $ do
                c <- acceptRequest pendingConn
                sendTextData c $ T.decodeUtf8 $ LBS.toStrict $ A.encode ("Accepted connection" :: T.Text)
                pure c

              forever $ do
                d <- liftIO $ receiveDataMessage conn
                incoming <- case d of
                  WS.Text b -> do
                    case A.decode b of
                      Just r -> pure r
                      _      -> throwM $ UnsupportedReceivedData d
                  _          -> throwM $ UnsupportedReceivedData d

                case incoming of
                  WSSubscribe sub ->
                    case sub of
                      WSSubNew  WSRPC{..} -> do
                        let onProgress r = do
                              print r
                              -- FIXME: Websocket only?
                              sendTextData conn $ T.decodeUtf8 $
                                LBS.toStrict $
                                A.encode $ WSNewProgress $ WSRPC
                                  { wsMethod   = "new"
                                  , wsParams   = NewProgress r
                                  , wsIdent    = wsIdent
                                  , wsInterval = 0
                                  , wsComplete = False
                                  , wsCancel   = False
                                  }

                        progresses <- envProgresses <$> ask
                        pConf      <- makeWalletProcessConfig
                        let mConf :: MakeWalletConfig
                            mConf = MakeWalletConfig
                                      { makeWalletName     = newName wsParams
                                      , makeWalletPassword = newPassword wsParams
                                      , makeWalletLanguage = toMoneroLanguage
                                                            $ newLanguage wsParams
                                      , makeWalletSeed     = newMnemonic wsParams
                                      , makeWalletInterval = wsInterval
                                      , makeWalletProgress = onProgress
                                      }
                        mnemonic' <- liftIO $ do
                          x <- async $ do
                                  putStrLn "Creating..."
                                  makeWallet pConf mConf
                                  threadDelay 1000000
                                  putStrLn "Opening..."
                                  (cfg,hs) <- openWallet pConf OpenWalletConfig
                                                { openWalletName     = newName wsParams
                                                , openWalletPassword = newPassword wsParams
                                                , openWalletInterval = wsInterval
                                                , openWalletProgress = onProgress
                                                }
                                  threadDelay 1000000
                                  putStrLn "Getting Seed..."
                                  (MoneroRPC.QueriedKey mn)
                                    <- MoneroRPC.queryKey cfg $ MoneroRPC.QueryKey MoneroRPC.KeyMnemonic
                                  putStrLn "Created."
                                  closeWallet hs
                                  pure mn
                          link x
                          stToIO $ modifySTRef' progresses $ Map.insert wsIdent x
                          mn <- wait x
                          stToIO $ modifySTRef' progresses $ Map.delete wsIdent
                          sendTextData conn $ T.decodeUtf8 $ LBS.toStrict $ A.encode $
                            WSComNew $ WSRPC
                              { wsMethod   = "new"
                              , wsParams   = mn
                              , wsIdent    = wsIdent
                              , wsInterval = 0
                              , wsComplete = True
                              , wsCancel   = False
                              }
                        configure $ \c@Config{configWallets} ->
                          c { configWallets = T.unpack (newName wsParams)
                                            : configWallets }
                      WSSubOpen WSRPC{..} -> do
                        -- FIXME: Check session password
                        let (w, OpenRequest{..}) = wsParams

                        Env{envOpenWallets} <- ask
                        mW <- liftIO $ stToIO $ Map.lookup w <$> readSTRef envOpenWallets
                        let onProgress r = do
                              print r
                              -- FIXME: Websocket only?
                              sendTextData conn $ T.decodeUtf8 $
                                LBS.toStrict $
                                A.encode $ WSOpenProgress $ WSRPC
                                  { wsMethod   = "new"
                                  , wsParams   = OpenProgress r
                                  , wsIdent    = wsIdent
                                  , wsInterval = 0
                                  , wsComplete = False
                                  , wsCancel   = False
                                  }

                        cfg <- case mW of
                          Just (cfg' :!: _) -> pure cfg'
                          Nothing -> do
                            pConf <- lift makeWalletProcessConfig
                            let oConf :: OpenWalletConfig
                                oConf = OpenWalletConfig
                                          { openWalletName     = w
                                          , openWalletPassword = openPassword
                                          , openWalletInterval = 1000000
                                          , openWalletProgress = onProgress
                                          }

                            liftIO $ do
                              -- FIXME: Parse logfile and stop blocking on predicate, too
                              (cfg',hs') <- openWallet pConf oConf
                              stToIO $ modifySTRef envOpenWallets $
                                Map.insert w (cfg' :!: hs')
                              pure cfg'

                        logInfoN $ "Found wallet: " <> w

                        r <- liftIO $ do
                          Monero.Balance
                            { balance         = b
                            , unlockedBalance = ub
                            }                    <- getBalance cfg
                          MoneroRPC.GotAddress a <- MoneroRPC.getAddress cfg
                          pure OpenResponse
                            { openBalance = Api.Balance
                                { balanceBalance  = fromIntegral b  / 1e12
                                , balanceUnlocked = fromIntegral ub / 1e12
                                }
                            , openAddress = a
                            , openHistory = []
                            , openHistoryMore = True
                            }

                        liftIO $ sendTextData conn $ T.decodeUtf8 $ LBS.toStrict
                               $ A.encode $ WSComOpen $ WSRPC
                                  { wsIdent = wsIdent
                                  , wsParams = r
                                  , wsMethod = "open"
                                  , wsComplete = True
                                  , wsCancel = False
                                  , wsInterval = 0
                                  }
                  WSSupply _ -> pure ()
  mid app req resp
