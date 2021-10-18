module Ergvein.Core.Worker.Height
  (
    heightWorker
  , updateWalletHeightBtc
  ) where

import Control.Lens
import Data.Functor
import Ergvein.Core.Node
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Types

heightWorker :: (MonadWallet t m, MonadStorage t m, MonadNode t m) => m ()
heightWorker = do
  updateWalletHeightBtc

updateWalletHeightBtc :: (MonadWallet t m, MonadStorage t m, MonadNode t m) => m ()
updateWalletHeightBtc = do
  mheightD <- getNodeHeightBtc
  void $ modifyPubStorage "heightWorkerBtc" $ ffor (updated mheightD) $ \case
    Nothing -> const Nothing
    Just h -> \ps -> Just $ ps & pubStorage'currencyPubStorages . ix BTC . currencyPubStorage'chainHeight .~ fromIntegral h
