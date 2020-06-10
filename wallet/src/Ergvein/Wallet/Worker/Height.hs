module Ergvein.Wallet.Worker.Height
  (
    heightAsking
  ) where

import Data.Time

import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Util

-- | Poll with the interval when everything is normal
defaulHeightPoll :: NominalDiffTime
defaulHeightPoll = 60

-- | Retry polling height each 5 seconds on errors
errorHeightPoll :: NominalDiffTime
errorHeightPoll = 5

heightAsking :: (MonadFrontAuth t m, MonadClient t m, MonadStorage t m) => m ()
heightAsking = void . widgetHoldDyn . fmap (traverse_ heightAsker) =<< getActiveCursD

heightAsker :: (MonadFrontAuth t m, MonadClient t m, MonadStorage t m) => Currency -> m ()
heightAsker cur = mdo
  let logPref = "[heightAsking][" <> showt cur <> "]:"
  logWrite $ logPref <> "Start worker"
  buildE <- getPostBuild
  timeD <- holdUniqDyn =<< holdDyn defaulHeightPoll timeE
  tickE <- fmap switchDyn $ widgetHoldDyn $ tickLossyFromPostBuildTime <$> timeD
  let goE = leftmost [() <$ tickE, buildE]
  resE <- getHeightRandom $ HeightRequest cur <$ goE
  let timeE = either (const errorHeightPoll) (const defaulHeightPoll) <$> resE
  setCurrentHeight cur . fmapMaybe id =<< performEvent (ffor resE $ \case
    Left err -> do
      logWrite $ logPref <> "Height request has failed: " <> showt err
      pure Nothing
    Right v -> pure $ Just $ fromIntegral $ heightRespHeight v
    )
