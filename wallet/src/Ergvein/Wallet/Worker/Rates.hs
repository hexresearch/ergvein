module Ergvein.Wallet.Worker.Rates
  (
    ratesWorker
  ) where

import Binance.Client.Types
import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Text
import Ergvein.Types.Fees
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import Ergvein.Types.Currency
import qualified Data.Set as S
import qualified Data.Map.Strict as M

ratesTimeout :: NominalDiffTime
ratesTimeout = 600

ratesWorker :: MonadFront t m => m ()
ratesWorker = do
  ratesRef  <- getRatesRef
  buildE  <- getPostBuild
  te      <- fmap void $ tickLossyFromPostBuildTime ratesTimeout
  tickE   <- delay 3 $ leftmost [te, buildE]
  settingsD <- getSettingsD
  let reqE = flip push tickE $ const $ do
        mRateSymbol <- fmap settingsRateSymbol $ sampleDyn settingsD
        pure $ ffor mRateSymbol $ \rs -> (BTC, MRatesRequest $ RatesRequest [rs])
  respE <- requestRandomIndexer reqE
  let ratesE = fforMaybe respE $ \case
        (_, MRatesResponse (RatesResponse rs)) -> Just rs
        _ -> Nothing
  performFork_ $ ffor ratesE $ \rs -> logWrite $ "Rates: " <> showt rs
  performFork_ $ ffor ratesE $ \rs -> modifyExternalRef_ ratesRef $ \rs' -> M.union rs rs'
