{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , Rank2Types
  , NamedFieldPuns
  #-}

module Main where

import Application
import Application.Types
import Main.Options
import Api

import Data.Default
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.String
import Data.Aeson as A

import Test.Tasty
import Test.Tasty.QuickCheck as Q
import Test.Tasty.HUnit
import Network.Wai.Test
import Network.Wai
import Network.Wai.Trans (runApplicationT)

import Control.Exception (catch, throw)
import qualified System.FilePath as F



main :: IO ()
main = do
  (env,cfg) <- digestAppOpts def
  print (env,cfg)
  let appTest :: (Env -> Mutable -> IO ()) -> IO ()
      appTest f = f env $ mkMutable cfg

      isVia :: String -> (BS.ByteString -> Env -> Mutable -> IO ()) -> TestTree
      isVia p t = testCase (fromString p) $ appTest $ t (fromString p)

  defaultMain $ testGroup "moneybit server"
    [ testGroup "server unit tests"
        [ testCase "create wallet" $
            let newTestWallet = NewRequest
                  { newName     = "test"
                  , newPassword = "asdf"
                  , newLanguage = English
                  , newMnemonic = Nothing
                  }
            in  appTest $ okPostTest newTestWallet "/new.json"
        , testCase "exists in config" $ do
            xs <- LBS.readFile (envWrkDir env F.</> "config.json")
            case A.decode xs of
              Nothing -> error "no config.json parse D:"
              Just Config{configWallets}
                | "test" `elem` configWallets -> pure () -- :D
              _ -> error "`test` isn't known!"
        , testCase "delete wallet" $ do
            let openTestWallet = OpenRequest
                  { openPassword = "asdf"
                  , openSessionPassword = ""
                  }

            appTest $ okPostTest openTestWallet "/wallet/test/open.json"
            appTest $ okTest "/wallet/test/delete.json"
            appTest $ authFailPostTest openTestWallet "/wallet/test/open.json"
        , testCase "doesn't exist in config" $ do
            xs <- LBS.readFile (envWrkDir env F.</> "config.json")
            case A.decode xs of
              Nothing -> error "no config.json parse D:"
              Just Config{configWallets}
                | "test" `elem` configWallets -> error "is known!"
              _ -> pure () -- :D
        , testCase "create wallet with mnemonic" $
            let newTestWallet = NewRequest
                  { newName     = "test"
                  , newPassword = "asdf"
                  , newLanguage = English
                  , newMnemonic = Just "gypsy annoyed renting delayed object ostrich vinegar suffice enigma excess paradise five ruling ulcers upon gotten eskimos unquoted plotting cinema jamming bimonthly skulls sleepless delayed" -- FIXME: use testnet server, from test.mnemonic
                  }
            in  appTest $ okPostTest newTestWallet "/new.json"
        , testCase "exists in config" $ do
            xs <- LBS.readFile (envWrkDir env F.</> "config.json")
            case A.decode xs of
              Nothing -> error "no config.json parse D:"
              Just Config{configWallets} | "test" `elem` configWallets -> pure () -- :D
              _ -> error "`test` isn't known!"
        , testCase "open wallet" $
            let openTestWallet = OpenRequest
                  { openPassword = "asdf"
                  , openSessionPassword = ""
                  }
            in  appTest $ okPostTest openTestWallet "/wallet/test/open.json"
        , testGroup "HTML"
            [ "/" `isVia` okTest
            , "/wallets.html" `isVia` okTest
            , "/wallet/test/overview.html" `isVia` okTest
            , "/wallet/test/transactions.html" `isVia` okTest
            ]
        , testGroup "JSON"
            [ testGroup "Wallet"
                [ "/wallet/test/integrated.json" `isVia` okTest
                , "/wallet/test/seeds.json" `isVia` okTest
                ]
            , testGroup "Utils"
                [ "/newPaymentId.json" `isVia` okTest
                ]
            ]
        , "/wallet/test/close.json" `isVia` okTest
        , testGroup "is closed"
            [ "/wallet/test/overview.html" `isVia` authFailTest
            , "/wallet/test/seeds.json" `isVia` authFailTest
            ]
        , testCase "Delete" $ do
            let openTestWallet = OpenRequest
                  { openPassword = "asdf"
                  , openSessionPassword = ""
                  }

            appTest $ okPostTest openTestWallet "/wallet/test/open.json"
            appTest $ okTest "/wallet/test/delete.json"
            appTest $ authFailPostTest openTestWallet "/wallet/test/open.json"
        , testCase "doesn't exist in config" $ do
            xs <- LBS.readFile (envWrkDir env F.</> "config.json")
            case A.decode xs of
              Nothing
                  -> error "no config.json parse D:"
              Just Config{configWallets}
                | "test" `elem` configWallets
                  -> error "is known!"
              _   -> pure () -- :D
        ]
    ]

moneybitSession :: Session () -> Env -> Mutable -> IO ()
moneybitSession xs env mut = do
  let app :: Application
      app = runApplicationT (runAppM env mut) Application.app
  runSession xs app

okTest :: BS.ByteString -> Env -> Mutable -> IO ()
okTest path = moneybitSession $ do
    indexResp <- request $ defaultRequest `setRawPathInfo` path
                                          `setPath` path
    assertStatus 200 indexResp

authFailTest :: BS.ByteString -> Env -> Mutable -> IO ()
authFailTest path = moneybitSession $ do
    indexResp <- request $ defaultRequest `setRawPathInfo` path
                                          `setPath` path
    assertStatus 401 indexResp


okPostTest :: ToJSON a => a -> BS.ByteString -> Env -> Mutable -> IO ()
okPostTest body path = moneybitSession $ do
    indexResp <- srequest SRequest
      { simpleRequest =
          jsonReq `setRawPathInfo` path
                         `setPath` path
      , simpleRequestBody = A.encode body
      }
    assertStatus 200 indexResp

authFailPostTest :: ToJSON a => a -> BS.ByteString -> Env -> Mutable -> IO ()
authFailPostTest body path = moneybitSession $ do
  indexResp <- srequest SRequest
    { simpleRequest =
          jsonReq `setRawPathInfo` path
                         `setPath` path
    , simpleRequestBody = A.encode body
    }
  assertStatus 401 indexResp



jsonReq :: Request
jsonReq = defaultRequest
  { requestHeaders = [("Accept","application/json")] }
