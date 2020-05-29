module Ergvein.Wallet.Worker.PeerDiscover where

import Data.Time
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Reflex.ExternalRef

infoWorkerInterval :: NominalDiffTime
infoWorkerInterval = 60

peerDiscoverWorker :: MonadFront t m => m ()
peerDiscoverWorker = do
  buildE <- getPostBuild
  indexerInfoRef  <- getActiveUrlsRef
  te <- void <$> tickLossyFromPostBuildTime infoWorkerInterval
  let goE = leftmost [void te, buildE]
  performFork_ $ ffor goE $ const $ do
   x <- readExternalRef indexerInfoRef 
   let x' = x
   pure ()
  pure ()