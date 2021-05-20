module Ergvein.Core.Worker(
    spawnWorkers
  , spawnPreWorkers
  , module Ergvein.Core.Worker.Discovery
  , module Ergvein.Core.Worker.Fees
  , module Ergvein.Core.Worker.Height
  , module Ergvein.Core.Worker.Indexer
  , module Ergvein.Core.Worker.Keys
  , module Ergvein.Core.Worker.Node
  , module Ergvein.Core.Worker.Rates
  , module Ergvein.Core.Worker.Store
  ) where

import Control.Monad (void)

import Ergvein.Core.Worker.Discovery
import Ergvein.Core.Worker.Fees
import Ergvein.Core.Worker.Height
import Ergvein.Core.Worker.Indexer
import Ergvein.Core.Worker.Keys
import Ergvein.Core.Worker.Node
import Ergvein.Core.Worker.Rates
import Ergvein.Core.Worker.Store

import Ergvein.Core.Node
import Ergvein.Core.Scan
import Ergvein.Core.Status
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Types.Storage
import Reflex.ExternalRef
import Reflex.Main.Thread

import qualified Data.Set as S

-- | Workers that spawns when user opens wallet
spawnWorkers :: (MonadStorage t m
  , MonadWallet t m
  , MonadNode t m
  , MonadStatus t m
  , MonadHasMain m
  , MonadSettings t m)
  => m ()
spawnWorkers = do
  setActiveCurrencies
  storeWorker
  scanner
  btcNodeController
  ratesWorker
  heightWorker
  feesWorker
  pubKeysGenerator
  pure ()

setActiveCurrencies :: (MonadStorage t m
  , MonadSettings t m
  , MonadWallet t m)
  => m ()
setActiveCurrencies = do
  buildE <- getPostBuild
  activeCurrencies <- _pubStorage'activeCurrencies <$> getPubStorage
  void . updateActiveCurs $ fmap (const . S.fromList) $ activeCurrencies <$ buildE

-- | Workers that spawns when user opens application but hasn't yet opened a wallet.
spawnPreWorkers :: (MonadClient t m, MonadHasMain m, MonadSettings t m)
  => m ()
spawnPreWorkers = do
  ensureErgveinNetwork
  indexerNodeController . S.toList =<< readExternalRef =<< getActiveAddrsRef
