{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Desktop.Run(

  ) where

import Ergvein.Wallet.Run
import Ergvein.Wallet.Run.Callbacks
import Control.Monad
import Control.Concurrent
import Data.Function

#if defined(MIN_VERSION_jsaddle_warp)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Language.Javascript.JSaddle (JSM)
import qualified Language.Javascript.JSaddle.Warp as JW
import System.Environment (lookupEnv)

instance PlatformRun where
  run jsm = do
    port <- maybe 3003 read <$> lookupEnv "JSADDLE_WARP_PORT"
    putStrLn $ "Running jsaddle-warp server on port " <> show port
    cbs <- noOpRunCallbacks
    void . forkIO $ fix $ \next -> do
      io <- readChan $ runUiCallbacks cbs
      io
      next
    JW.run port $ jsm cbs
#elif defined(MIN_VERSION_jsaddle_wkwebview)
#if defined(ios_HOST_OS)
import Data.Default
import Data.Monoid ((<>))
import Language.Javascript.JSaddle (JSM)
import Language.Javascript.JSaddle.WKWebView (run', mainBundleResourcePath)
import Language.Javascript.JSaddle.WKWebView.Internal (jsaddleMainHTMLWithBaseURL)

-- TODO: upstream to jsaddle-wkwebview
instance PlatformRun where
  run jsm = do
    let indexHtml = "<!DOCTYPE html><html><head></head><body></body></html>"
    baseUrl <- mainBundleResourcePath >>= \case
      Nothing -> do
        putStrLn "Reflex.Dom.run: unable to find main bundle resource path. Assets may not load properly."
        return ""
      Just p -> return $ "file://" <> p <> "/index.html"
    cbs <- noOpRunCallbacks
    void . forkIO $ fix $ \next -> do
      io <- readChan $ runUiCallbacks cbs
      io
      next
    run' def $ jsaddleMainHTMLWithBaseURL indexHtml baseUrl $ jsm cbs
#else
import qualified Language.Javascript.JSaddle.WKWebView as WB (run)

instance PlatformRun where
  run = WB.run

#endif
#else
import qualified Language.Javascript.JSaddle.WebKitGTK as WB (run)
import Language.Javascript.JSaddle.Types

instance PlatformRun where
  run jsm = do
    cbs <- noOpRunCallbacks
    void . forkIO $ fix $ \next -> do
      io <- readChan $ runUiCallbacks cbs
      io
      next
    WB.run $ jsm cbs
#endif
