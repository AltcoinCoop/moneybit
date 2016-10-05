{-# LANGUAGE
    OverloadedStrings
, ScopedTypeVariables
  #-}

module Main where

import Application
import Application.Types
import Main.Options
import Api

import Data.Default
import qualified Data.ByteString as BS
import Data.String
import Data.Aeson as A

import Test.Tasty
import Test.Tasty.QuickCheck as Q
import Test.Tasty.HUnit
import Network.Wai.Test
import Network.Wai
import Network.Wai.Trans (runApplicationT)

import Control.Exception (catch)



main :: IO ()
main = do
  (env,cfg) <- digestAppOpts def
  let appTest f = f env $ mkMutable cfg
      isVia p t = testCase (fromString p) $ appTest $ t (fromString p)

  defaultMain $ testGroup "moneybit server"
    [ testGroup "server unit tests"
        [ testCase "create wallet" $
            let newTestWallet = NewRequest
                  { newName     = "test"
                  , newPassword = "asdf"
                  , newLanguage = English
                  }
            in  appTest $ okPostTest newTestWallet "/new.json"
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
            , "/wallet/test/close.json" `isVia` okTest
            ]
        , testGroup "Closed"
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
          let req = defaultRequest `setRawPathInfo` path
                                          `setPath` path
          in  req { requestHeaders = [("Accept","application/json")] }
      , simpleRequestBody = A.encode body
      }
    assertStatus 200 indexResp

authFailPostTest :: ToJSON a => a -> BS.ByteString -> Env -> Mutable -> IO ()
authFailPostTest body path = moneybitSession $ do
  indexResp <- srequest SRequest
    { simpleRequest =
          let req = defaultRequest `setRawPathInfo` path
                                          `setPath` path
          in  req { requestHeaders = [("Accept","application/json")] }
    , simpleRequestBody = A.encode body
    }
  assertStatus 401 indexResp
