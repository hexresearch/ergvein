module Ergvein.Wallet.Run(
    run
  ) where

import Android.HaskellActivity
import Control.Monad
import Control.Concurrent
import Data.Default
import Data.String
import Ergvein.Wallet.Native.Android.MainWidget
import System.IO
import Language.Javascript.JSaddle (JSM)

run :: (RunCallbacks -> JSM ()) -> IO ()
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
