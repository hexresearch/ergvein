module Ergvein.Wallet.Worker.NodeDiscovery
  ( ensureErgveinNetwork
  , restoreNetwork
  ) where

import Control.Monad.IO.Class
import Data.Maybe
import Reflex.ExternalRef
import System.Random.Shuffle

import Ergvein.Node.Resolve
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Util

import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE

operableNetworkSize, targetNetworkSize :: Int
operableNetworkSize | isTestnet = 1
                    | otherwise = 2
targetNetworkSize = 16

ensureErgveinNetwork :: (MonadHasUI m, MonadIndexClient t m, MonadHasSettings t m) => m ()
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

restoreNetwork :: (MonadHasUI m, MonadIndexClient t m, MonadHasSettings t m) => Event t () -> m ()
restoreNetwork e = do
  activsRef <- getActiveAddrsRef
  activeAddressesAmountE <- performEvent $ ffor e $ const $ length <$> readExternalRef activsRef
  restoreNetworkAmount activeAddressesAmountE

restoreNetworkAmount :: (MonadHasUI m, MonadIndexClient t m, MonadHasSettings t m) => Event t Int -> m ()
restoreNetworkAmount notOperablePeerAmountE = do
  rs <- mkResolvSeed
  reloadedFromSeedE <- performFork $ ffor notOperablePeerAmountE $ \presentAmount -> do
    maybeAddresses <- liftIO $ dnsLookupAddrs rs defIndexerPort defaultSeedNodesSource
    addresses <- case maybeAddresses of
      Just addrs -> pure $ NE.toList addrs
      _          -> resolveAddrs rs defIndexerPort defaultIndexers
     
    let neededSet = take (targetNetworkSize - presentAmount) addresses
    pure neededSet
  void $ activateURLList reloadedFromSeedE