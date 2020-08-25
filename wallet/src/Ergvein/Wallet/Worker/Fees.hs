module Ergvein.Wallet.Worker.Fees
  (
    feesWorker
  ) where

import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types
import Ergvein.Types.Fees
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Util

import qualified Ergvein.Types.Currency as ETC
import qualified Data.Set as S
import qualified Data.Map.Strict as M

feesTimeout :: NominalDiffTime
feesTimeout = 60

feesWorker :: MonadFront t m => m ()
feesWorker = do
  feeRef  <- getFeesRef
  cursD   <- getActiveCursD
  buildE  <- getPostBuild
  te      <- fmap void $ tickLossyFromPostBuildTime feesTimeout
  tickE   <- delay 1 $ leftmost [te, void $ updated cursD, buildE]
  let goE = attachWith (\cs _ -> fmap currencyToCurrencyCode $ S.toList cs) (current cursD) tickE
  respE <- requestRandomIndexer $ MFeeRequest <$> goE
  let feesE = fforMaybe respE $ \case
        MFeeResponse fees -> Just $ repack fees
        _ -> Nothing
  performFork_ $ ffor feesE $ \fm -> modifyExternalRef_ feeRef $ \fm' -> M.union fm fm'
  where
    repack :: [FeeResp] -> M.Map ETC.Currency FeeBundle
    repack fees = M.fromList $ ffor fees $ \case
      FeeRespBTC _ bndl -> (ETC.BTC, bndl)
      FeeRespGeneric cur h m l -> let
        bndl = FeeBundle (h,h) (m,m) (l,l)
        in (currencyCodeToCurrency cur, bndl)
