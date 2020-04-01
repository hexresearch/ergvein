{-# LANGUAGE CPP #-}
module Ergvein.Wallet.Main(
    frontend
  , mainWidgetWithCss
  ) where

import Control.Monad.IO.Class
import Data.Time 
import Ergvein.Index.API.Types
import Ergvein.Text 
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
import Ergvein.Wallet.Util
import Reflex.ExternalRef 

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

-- | Poll with the interval when everything is normal
defaulHeightPoll :: NominalDiffTime 
defaulHeightPoll = 60 

-- | Retry polling height each 5 seconds on errors
errorHeightPoll :: NominalDiffTime
errorHeightPoll = 5

-- | TODO: stop using indexer and start quering nodes directly
heightAsking :: MonadFrontBase t m => m ()
heightAsking = do 
  be <- getPostBuild
  pollRef <- newExternalRef defaulHeightPoll
  pollDyn <- externalRefDynamic pollRef
  te <- fmap (switch . current) $ widgetHoldDyn $ tickLossyFromPostBuildTime <$> pollDyn
  let e = leftmost [void te, be]
      queryHeights c = do 
        resE <- getHeight $ HeightRequest c <$ e 
        performEvent_ $ fforMaybe resE $ \case 
          Left er -> Just $ logWrite $ "Height request for " <> showt c <> " is failed: " <> showt er 
          _ -> Nothing
        performEvent_ $ ffor resE $ liftIO . writeExternalRef pollRef . either (const errorHeightPoll) (const defaulHeightPoll)
        he <- handleDangerMsg resE
        setCurrentHeight c $ fromIntegral . heightRespHeight <$> he
  traverse_  queryHeights allCurrencies
