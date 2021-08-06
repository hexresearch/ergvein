module Ergvein.Core.Worker.Rates
  (
    ratesWorker
  ) where

import Data.Maybe
import Data.Time
import Data.Functor
import Ergvein.Core.Currency
import Ergvein.Core.Settings
import Ergvein.Core.Node.Monad
import Ergvein.Core.Wallet.Monad
import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Text
import Ergvein.Types.Currency
import Reflex.ExternalRef
import Reflex.Fork
import Reflex.Flunky
import Sepulcas.Native

import qualified Data.Map.Strict as M

ratesTimeout :: NominalDiffTime
ratesTimeout = 600

ratesWorker :: (MonadSettings t m, MonadWallet t m, MonadNode t m) => m ()
ratesWorker = do
  ratesRef  <- getRatesRef
  mRateD <- getFiatRateSettings
  let fiatsD = ffor mRateD maybeToList
  let btcCC = currencyToCurrencyCode BTC
  void $ networkHoldDyn $ ffor fiatsD $ \case
    [] -> pure ()
    fs -> do
      buildE  <- getPostBuild
      te      <- void <$> tickLossyFromPostBuildTime ratesTimeout
      tickE   <- delay 2 $ leftmost [te, buildE]
      let reqE = (BTC, MRatesRequest $ RatesRequest $ M.singleton btcCC fs) <$ tickE
      respE <- requestRandomIndexer reqE
      let ratesE = fforMaybe respE $ \case
            (_, MRatesResponse (RatesResponse rs)) -> Just $ M.mapKeys currencyCodeToCurrency rs
            _ -> Nothing
      performFork_ $ ffor ratesE $ \rs -> logWrite $ "Rates: " <> showt rs
      performFork_ $ ffor ratesE $ \rs -> modifyExternalRef_ ratesRef $ \rs' -> M.union rs rs'
