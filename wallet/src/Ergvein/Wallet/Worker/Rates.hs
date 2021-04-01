module Ergvein.Wallet.Worker.Rates
  (
    ratesWorker
  ) where

import Data.Maybe
import Data.List (nub)
import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Text
import Ergvein.Types.Fees
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Sepulcas.Native
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
  settingsD <- getSettingsD
  mFiatD <- holdUniqDyn $ fmap settingsFiatCurr settingsD
  mRateD <- holdUniqDyn $ fmap settingsRateFiat settingsD
  let fiatsD = (\a b -> nub $ catMaybes [a,b]) <$> mFiatD <*> mRateD
  let btcCC = currencyToCurrencyCode BTC
  void $ widgetHoldDyn $ ffor fiatsD $ \case
    [] -> pure ()
    fs -> do
      buildE  <- getPostBuild
      te      <- fmap void $ tickLossyFromPostBuildTime ratesTimeout
      tickE   <- delay 2 $ leftmost [te, buildE]
      let reqE = (BTC, MRatesRequest $ RatesRequest $ M.singleton btcCC fs) <$ tickE
      respE <- requestRandomIndexer reqE
      let ratesE = fforMaybe respE $ \case
            (_, MRatesResponse (RatesResponse rs)) -> Just $ M.mapKeys currencyCodeToCurrency rs
            _ -> Nothing
      performFork_ $ ffor ratesE $ \rs -> logWrite $ "Rates: " <> showt rs
      performFork_ $ ffor ratesE $ \rs -> modifyExternalRef_ ratesRef $ \rs' -> M.union rs rs'
