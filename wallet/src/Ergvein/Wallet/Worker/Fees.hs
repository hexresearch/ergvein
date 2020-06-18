module Ergvein.Wallet.Worker.Fees
  (
    feesWorker
  ) where

import Data.Time
import Reflex.ExternalRef

import Ergvein.Index.API.Types
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Util

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
  let tickE = leftmost [te, buildE]
  efeesE <- getFeeEstimatesRandom $ attachWith (\cs _ -> S.toList cs) (current cursD) tickE
  let feesE = fmapMaybe (either (const Nothing) (Just . indexFeesRespFees)) efeesE
  performFork_ $ ffor feesE $ \fm -> modifyExternalRef_ feeRef $ \fm' -> M.union fm fm'
