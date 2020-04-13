module Ergvein.Wallet.Worker.Height
  (
    heightAsking
  ) where

import Control.Monad.IO.Class
import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Alert
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Util

-- | Poll with the interval when everything is normal
defaulHeightPoll :: NominalDiffTime
defaulHeightPoll = 60

-- | Retry polling height each 5 seconds on errors
errorHeightPoll :: NominalDiffTime
errorHeightPoll = 5

-- | TODO: stop using indexer and start quering nodes directly
heightAsking :: (MonadFrontAuth t m, MonadClient t m) => m ()
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
  traverse_ queryHeights allCurrencies
