module Ergvein.Core.Worker.Fees
  (
    feesWorker
  ) where

import Data.Functor
import Data.Time
import Ergvein.Core.Currency
import Ergvein.Core.Node.Monad
import Ergvein.Core.Wallet.Monad
import Ergvein.Index.Protocol.Types
import Ergvein.Types.Fees
import Reflex.ExternalRef
import Reflex.Fork

import qualified Ergvein.Types.Currency as ETC
import qualified Data.Set as S
import qualified Data.Map.Strict as M

feesTimeout :: NominalDiffTime
feesTimeout = 60

feesWorker :: (MonadWallet t m, MonadNode t m) => m ()
feesWorker = do
  feeRef  <- getFeesRef
  cursD   <- getActiveCursD
  buildE  <- getPostBuild
  te      <- fmap void $ tickLossyFromPostBuildTime feesTimeout
  tickE   <- delay 1 $ leftmost [te, void $ updated cursD, buildE]
  let goE = attachWith (\cs _ -> fmap currencyToCurrencyCode $ S.toList cs) (current cursD) tickE
  respE <- requestRandomIndexer $ ((ETC.BTC, ) . MFeeRequest) <$> goE -- TODO: Fix this for multiple currencies
  let feesE = fforMaybe respE $ \case
        (_, MFeeResponse fees) -> Just $ repack fees
        _ -> Nothing
  performFork_ $ ffor feesE $ \fm -> modifyExternalRef_ feeRef $ \fm' -> M.union fm fm'
  where
    repack :: [FeeResp] -> M.Map ETC.Currency FeeBundle
    repack fees = M.fromList $ ffor fees $ \case
      FeeRespBTC _ bndl -> (ETC.BTC, bndl)
      FeeRespGeneric cur h m l -> let
        bndl = FeeBundle (h,h) (m,m) (l,l)
        in (currencyCodeToCurrency cur, bndl)
