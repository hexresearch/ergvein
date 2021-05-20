module Sepulcas.Android.Run(

  ) where

import Android.HaskellActivity
import Control.Concurrent
import Control.Monad
import Data.Default
import Data.IORef
import Data.String
import Language.Javascript.JSaddle (JSM)
import Sepulcas.Android.Native.MainWidget
import Sepulcas.Run
import Sepulcas.Run.Callbacks
import System.IO

instance PlatformRun where
  run jsm = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    pauseCbRef <- newIORef $ pure ()
    resumeCbRef <- newIORef $ pure ()
    backCbRef <- newIORef $ pure ()
    uiCbChan <- newChan
    let cbs = RunCallbacks {
            runPauseCallback = pauseCbRef
          , runResumeCallback = resumeCbRef
          , runBackCallback = backCbRef
          , runUiCallbacks = uiCbChan
          }
    continueWithCallbacks $ def
      { _activityCallbacks_onCreate = \_ -> do
          a <- getHaskellActivity
          let startPage = fromString "file:///android_asset/index.html"
          startMainWidget a uiCbChan startPage $ jsm cbs
      , _activityCallbacks_onPause = do
          io <- readIORef pauseCbRef
          io
      , _activityCallbacks_onResume = do
          io <- readIORef resumeCbRef
          io
      , _activityCallbacks_onBackPressed = do
          io <- readIORef backCbRef
          io
      }
    forever $ threadDelay 1000000000
