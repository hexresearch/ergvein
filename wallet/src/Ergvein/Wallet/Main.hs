{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Ergvein.Index.API.Types
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Alert.Handler
import Ergvein.Wallet.Client
import Ergvein.Wallet.Loading
import Ergvein.Wallet.Log.Writer
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Native
import Ergvein.Wallet.Page.Balances
import Ergvein.Wallet.Page.Initial
import Ergvein.Wallet.Password
import Ergvein.Wallet.Scan

import Reflex.Dom.Main (mainWidgetWithCss)

frontend :: MonadFrontBase t m => m ()
frontend = do
  logWrite "Frontend started"
  alertHandlerWidget
  loadingWidget
  heightAsking
#ifdef ANDROID
  askPatternModal
#else
  askPasswordModal
#endif
  logWriter =<< fmap fst getLogsTrigger
  logWrite "Entering initial page"
  void $ retractStack initialPage `liftAuth` (accountDiscovery >> retractStack balancesPage)

-- | TODO: stop using indexer and start quering nodes directly
heightAsking :: MonadFrontBase t m => m ()
heightAsking = do 
  be <- getPostBuild
  te <- tickLossyFromPostBuildTime 60
  let e = leftmost [void te, be]
  traverse_ (queryHeights e) allCurrencies
  where
    queryHeights e c = do 
      resE <- getHeight $  HeightRequest c <$ e 
      he <- handleDangerMsg resE
      setCurrentHeight c $ fromIntegral . heightRespHeight <$> he

