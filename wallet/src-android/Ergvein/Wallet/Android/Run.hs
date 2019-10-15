module Ergvein.Wallet.Android.Run(

  ) where

import Android.HaskellActivity
import Control.Concurrent
import Control.Monad
import Data.Default
import Data.String
import Ergvein.Wallet.Native.Android.MainWidget
import Ergvein.Wallet.Run
import Language.Javascript.JSaddle (JSM)
import System.IO

instance PlatformRun where
  run jsm = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    backCbRef <- newIORef $ pure ()
    uiCbChan <- newChan
    let cbs = RunCallbacks {
            runBackCallback = backCbRef
          , runUiCallbacks = uiCbChan
          }
    continueWithCallbacks $ def
      { _activityCallbacks_onCreate = \_ -> do
          a <- getHaskellActivity
          let startPage = fromString "file:///android_asset/index.html"
          startMainWidget a uiCbChan startPage $ jsm cbs
      , _activityCallbacks_onBackPressed = do
          io <- readIORef backCbRef
          io
      }
    forever $ threadDelay 1000000000
