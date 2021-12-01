{-# OPTIONS_GHC -Wno-orphans #-}
module Ergvein.Core.Client.Env(
    ClientEnv(..)
  , ClientM
  , HasClientEnv(..)
  , newClientEnv
  , runClient
  ) where

import Control.Monad.Reader
import Data.Map (Map)
import Data.Time
import Ergvein.Core.Client.Monad
import Ergvein.Core.Resolve
import Ergvein.Core.Settings
import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Reflex
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Main.Thread

import qualified Data.Map.Strict as M
import qualified Data.Set as S

data ClientEnv t = ClientEnv {
  cenv'addrsArchive    :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, cenv'inactiveAddrs   :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, cenv'activeAddrs     :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, cenv'indexConmap     :: !(ExternalRef t (Map ErgveinNodeAddr (IndexerConnection t)))
, cenv'indexStatus     :: !(ExternalRef t (Map ErgveinNodeAddr IndexerStatus))
, cenv'timeout         :: !(ExternalRef t NominalDiffTime)
, cenv'indexReqSel     :: !(IndexReqSelector t)
, cenv'indexReqFire    :: !(Map ErgveinNodeAddr IndexerMsg -> IO ())
, cenv'activateIndexEF :: !(EventTrigger t [ErgveinNodeAddr])
}

type ClientM t m = ReaderT (ClientEnv t) m

class Monad m => HasClientEnv t m | m -> t where
  getClientEnv :: m (ClientEnv t)

instance Monad m => HasClientEnv t (ClientM t m) where
  getClientEnv = ask
  {-# INLINE getClientEnv #-}

instance (HasClientEnv t m, MonadClientConstr t m) => MonadClient t m where
  getActiveAddrsRef = fmap cenv'activeAddrs getClientEnv
  {-# INLINE getActiveAddrsRef #-}
  getArchivedAddrsRef = fmap cenv'addrsArchive getClientEnv
  {-# INLINE getArchivedAddrsRef #-}
  getActiveConnsRef = fmap cenv'indexConmap getClientEnv
  {-# INLINE getActiveConnsRef #-}
  getStatusConnsRef = fmap cenv'indexStatus getClientEnv
  {-# INLINE getStatusConnsRef #-}
  getInactiveAddrsRef = fmap cenv'inactiveAddrs getClientEnv
  {-# INLINE getInactiveAddrsRef #-}
  getRequestTimeoutRef = fmap cenv'timeout getClientEnv
  {-# INLINE getRequestTimeoutRef #-}
  getIndexReqSelector = fmap cenv'indexReqSel getClientEnv
  {-# INLINE getIndexReqSelector #-}
  getIndexReqFire = fmap cenv'indexReqFire getClientEnv
  {-# INLINE getIndexReqFire #-}
  getActivationEF = fmap (triggerPair . cenv'activateIndexEF) getClientEnv
  {-# INLINE getActivationEF #-}

newClientEnv :: (MonadHasMain m, MonadSettings t m) => m (ClientEnv t)
newClientEnv = do
  settings <- getSettings
  rs <- mkResolvSeed
  socadrs         <- fmap namedAddrName <$> resolveAddrs rs defIndexerPort (settingsActiveAddrs settings)
  urlsArchive     <- newExternalRef . S.fromList . fmap namedAddrName =<< resolveAddrs rs defIndexerPort (settingsArchivedAddrs settings)
  inactiveUrls    <- newExternalRef . S.fromList . fmap namedAddrName =<< resolveAddrs rs defIndexerPort (settingsDeactivatedAddrs settings)
  activeAddrsRef  <- newExternalRef $ S.fromList socadrs
  indexConmapRef  <- newExternalRef M.empty
  indexStatusRef  <- newExternalRef M.empty
  timeoutRef      <- newExternalRef $ settingsReqTimeout settings
  (iReqE, iReqFire) <- newTriggerEvent
  let indexSel = fanMap iReqE
  indexEF <- newTriggerEvent'
  ClientEnv
    <$> pure urlsArchive
    <*> pure inactiveUrls
    <*> pure activeAddrsRef
    <*> pure indexConmapRef
    <*> pure indexStatusRef
    <*> pure timeoutRef
    <*> pure indexSel
    <*> pure iReqFire
    <*> pure indexEF

runClient :: ClientEnv t -> ClientM t m a -> m a
runClient = flip runReaderT
