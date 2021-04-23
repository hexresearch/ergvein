module Ergvein.Core.Worker.Discovery
  ( ensureErgveinNetwork
  , restoreNetwork
  ) where

import Control.Monad.IO.Class
import Data.Functor
import Ergvein.Core.Client
import Ergvein.Core.Resolve
import Ergvein.Core.Settings
import Ergvein.Core.Wallet.Monad
import Ergvein.Node.Resolve
import Reflex.ExternalRef
import Reflex.Fork
import Reflex.Main.Thread

import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

targetNetworkSize :: Int
targetNetworkSize = 16

ensureErgveinNetwork :: (MonadHasMain m, MonadClient t m, MonadSettings t m) => m ()
ensureErgveinNetwork = do
  iaRef     <- getInactiveAddrsRef
  acrhRef   <- getArchivedAddrsRef
  activsRef <- getActiveAddrsRef
  buildE <- getPostBuild
  addressesAmountE <- performEvent $ ffor buildE $ const $ do
    ia <- readExternalRef iaRef
    acrh <- readExternalRef acrhRef
    activs <- readExternalRef activsRef
    pure $ length $ S.unions [ia, acrh, activs]
  let noAddressesE = ffilter (== 0) addressesAmountE
  restoreNetworkAmount noAddressesE

restoreNetwork :: (MonadHasMain m, MonadClient t m, MonadSettings t m) => Event t () -> m ()
restoreNetwork e = do
  activsRef <- getActiveAddrsRef
  activeAddressesAmountE <- performEvent $ ffor e $ const $ length <$> readExternalRef activsRef
  restoreNetworkAmount activeAddressesAmountE

restoreNetworkAmount :: (MonadHasMain m, MonadClient t m, MonadSettings t m) => Event t Int -> m ()
restoreNetworkAmount notOperablePeerAmountE = do
  rs <- mkResolvSeed
  reloadedFromSeedE <- performFork $ ffor notOperablePeerAmountE $ \presentAmount -> do
    maybeAddresses <- liftIO $ dnsLookupAddrs rs defIndexerPort defaultSeedNodesSource
    addresses <- case maybeAddresses of
      Just addrs -> pure $ NE.toList addrs
      _          -> resolveAddrs rs defIndexerPort defaultIndexers

    let neededSet = take (targetNetworkSize - presentAmount) $ namedAddrName <$> addresses
    pure neededSet
  void $ activateURLList reloadedFromSeedE
