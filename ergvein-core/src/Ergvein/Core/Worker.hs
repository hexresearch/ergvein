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

import Control.Monad (void, when)

import Ergvein.Core.Worker.Discovery
import Ergvein.Core.Worker.Fees
import Ergvein.Core.Worker.Height
import Ergvein.Core.Worker.Indexer
import Ergvein.Core.Worker.Keys
import Ergvein.Core.Worker.Mempool
import Ergvein.Core.Worker.Node
import Ergvein.Core.Worker.Rates
import Ergvein.Core.Worker.Store

import Ergvein.Core.Node
import Ergvein.Core.Scan
import Ergvein.Core.Status
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Types.Storage
import Ergvein.Types.Currency
import Reflex.ExternalRef
import Reflex.Flunky
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
  cursD <- getActiveCursD
  btcD <- holdUniqDyn $ S.member BTC <$> cursD

  -- Common workers
  setActiveCurrencies
  storeWorker
  ratesWorker

  -- BTC only workers
  _ <- networkHoldDyn $ ffor btcD $ \b -> when b $ do
    btcMempoolWorker
    feesWorker
    pubKeysGeneratorBtc
    scanner
    btcNodeController
    updateWalletHeightBtc
    pure ()
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
