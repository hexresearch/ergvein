module Ergvein.Wallet.Worker.Rates
  (
    ratesWorker
  ) where

import Binance.Client.Types
import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types
import Ergvein.Text
import Ergvein.Types.Fees
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Util

import qualified Ergvein.Types.Currency as ETC
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
  respE <- requestRandomIndexer $ (ETC.BTC, MRatesRequest $ RatesRequest [BTCUSDT, BTCBUSD]) <$ tickE
  let ratesE = fforMaybe respE $ \case
        (_, MRatesResponse (RatesResponse rs)) -> Just rs
        _ -> Nothing
  performFork_ $ ffor ratesE $ \rs -> logWrite $ "Rates: " <> showt rs
  performFork_ $ ffor ratesE $ \rs -> modifyExternalRef_ ratesRef $ \rs' -> M.union rs rs'
  where
    repack :: [FeeResp] -> M.Map ETC.Currency FeeBundle
    repack fees = M.fromList $ ffor fees $ \case
      FeeRespBTC _ bndl -> (ETC.BTC, bndl)
      FeeRespGeneric cur h m l -> let
        bndl = FeeBundle (h,h) (m,m) (l,l)
        in (currencyCodeToCurrency cur, bndl)
