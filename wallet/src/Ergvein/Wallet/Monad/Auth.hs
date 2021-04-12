{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UndecidableInstances #-}

module Ergvein.Wallet.Monad.Auth(
    liftAuth
  , liftUnauthed
  ) where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Lens
import Control.Monad.Reader
import Control.Monad.STM
import Crypto.Random.Types
import Data.Fixed
import Data.Map.Strict (Map)
import Data.Text as T
import Data.Time (NominalDiffTime)
import Network.Socket
import Reflex
import Reflex.Network
import Reflex.Dom.Retractable
import Reflex.ExternalRef

import Ergvein.Node.Constants
import Ergvein.Types.WalletInfo
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Keys
import Ergvein.Types.Network
import Ergvein.Types.Storage
import Ergvein.Types.Transaction (BlockHeight)
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Sepulcas.Native
import Sepulcas.Monad
import Ergvein.Wallet.Node
import Ergvein.Wallet.Settings (Settings(..))
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Version

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V

data Env t = Env {
  -- Unauth context's fields
  env'settings        :: !(ExternalRef t Settings)
, env'sepulca         :: !(Sepulca t)
-- Auth context
, env'authRef         :: !(ExternalRef t WalletInfo)
, env'logoutFire      :: !(IO ())
, env'activeCursRef   :: !(ExternalRef t (S.Set Currency))
, env'filtersHeights  :: !(ExternalRef t (Map Currency BlockHeight))
, env'statusUpdates    :: !(ExternalRef t (Map Currency StatusUpdate))
, env'filtersSyncRef  :: !(ExternalRef t (Map Currency Bool))
, env'nodeConsRef     :: !(ExternalRef t (ConnMap t))
, env'nodeReqSelector :: !(NodeReqSelector t)
, env'nodeReqFire     :: !(Map Currency (Map SockAddr NodeMessage) -> IO ())
, env'feesStore       :: !(ExternalRef t (Map Currency FeeBundle))
, env'storeMutex      :: !(MVar ())
, env'storeChan       :: !(TChan (Text, WalletInfo))
, env'ratesRef        :: !(ExternalRef t (Map Currency (Map Fiat Centi)))
-- Client context
, env'addrsArchive    :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, env'inactiveAddrs   :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, env'activeAddrs     :: !(ExternalRef t (S.Set ErgveinNodeAddr))
, env'indexConmap     :: !(ExternalRef t (Map ErgveinNodeAddr (IndexerConnection t)))
, env'indexStatus     :: !(ExternalRef t (Map ErgveinNodeAddr IndexerStatus))
, env'reqUrlNum       :: !(ExternalRef t (Int, Int))
, env'actUrlNum       :: !(ExternalRef t Int)
, env'timeout         :: !(ExternalRef t NominalDiffTime)
, env'indexReqSel     :: !(IndexReqSelector t)
, env'indexReqFire    :: !(Map ErgveinNodeAddr IndexerMsg -> IO ())
, env'activateIndexEF :: !(Event t [ErgveinNodeAddr], [ErgveinNodeAddr] -> IO ())
}

type ErgveinM t m = ReaderT (Env t) m

instance Monad m => HasSepulca t (ErgveinM t m) where
  getSepulca = asks env'sepulca
  {-# INLINE getSepulca #-}

instance (MonadFrontConstr t m, MonadRetract t m, PlatformNatives, HasVersion) => MonadFrontBase t (ErgveinM t m) where
  getWalletInfoMaybeRef = fmapExternalRef Just =<< asks env'authRef
  {-# INLINE getWalletInfoMaybeRef #-}

  setWalletInfo e = do
    authRef <- asks env'authRef
    fire <- asks env'logoutFire
    performEvent $ ffor e $ \case
      Nothing -> do
        logWrite "authed setWalletInfo: logout"
        setLastStorage Nothing
        liftIO fire
      Just v -> do
        logWrite "authed setWalletInfo: changing auth info"
        setLastStorage $ Just . _storage'walletName . _walletInfo'storage $ v
        writeExternalRef authRef v
  {-# INLINE setWalletInfo #-}

instance MonadBaseConstr t m => MonadHasSettings t (ErgveinM t m) where
  getSettingsRef = asks env'settings
  {-# INLINE getSettingsRef #-}

instance MonadFrontBase t m => MonadFrontAuth t (ErgveinM t m) where
  getStatusUpdRef = asks env'statusUpdates
  {-# INLINE getStatusUpdRef #-}
  getFiltersSyncRef = asks env'filtersSyncRef
  {-# INLINE getFiltersSyncRef #-}
  getActiveCursRef = asks env'activeCursRef
  {-# INLINE getActiveCursRef #-}
  getWalletInfoRef = asks env'authRef
  {-# INLINE getWalletInfoRef #-}
  getNodeConnRef = asks env'nodeConsRef
  {-# INLINE getNodeConnRef #-}
  getNodeNodeReqSelector = asks env'nodeReqSelector
  {-# INLINE getNodeNodeReqSelector #-}
  getFeesRef = asks env'feesStore
  {-# INLINE getFeesRef #-}
  getRatesRef = asks env'ratesRef
  {-# INLINE getRatesRef #-}
  getNodeReqFire = asks env'nodeReqFire
  {-# INLINE getNodeReqFire #-}

instance MonadBaseConstr t m => MonadIndexClient t (ErgveinM t m) where
  getActiveAddrsRef = asks env'activeAddrs
  {-# INLINE getActiveAddrsRef #-}
  getArchivedAddrsRef = asks env'addrsArchive
  {-# INLINE getArchivedAddrsRef #-}
  getActiveConnsRef = asks env'indexConmap
  {-# INLINE getActiveConnsRef #-}
  getStatusConnsRef = asks env'indexStatus
  {-# INLINE getStatusConnsRef #-}
  getInactiveAddrsRef = asks env'inactiveAddrs
  {-# INLINE getInactiveAddrsRef #-}
  getActiveUrlsNumRef = asks env'actUrlNum
  {-# INLINE getActiveUrlsNumRef #-}
  getRequiredUrlNumRef = asks env'reqUrlNum
  {-# INLINE getRequiredUrlNumRef #-}
  getRequestTimeoutRef = asks env'timeout
  {-# INLINE getRequestTimeoutRef #-}
  getIndexReqSelector = asks env'indexReqSel
  {-# INLINE getIndexReqSelector #-}
  getIndexReqFire = asks env'indexReqFire
  {-# INLINE getIndexReqFire #-}
  getActivationEF = asks env'activateIndexEF
  {-# INLINE getActivationEF #-}

instance MonadBaseConstr t m => MonadAlertPoster t (ErgveinM t m) where
  postAlert e = do
    (_, fire) <- asks (sepulca'alertsEF . env'sepulca)
    performEvent_ $ liftIO . fire <$> e
  newAlertEvent = asks (fst . sepulca'alertsEF .  env'sepulca)
  getAlertEventFire = asks (sepulca'alertsEF . env'sepulca)
  {-# INLINE postAlert #-}
  {-# INLINE newAlertEvent #-}
  {-# INLINE getAlertEventFire #-}

instance (MonadBaseConstr t m, HasStoreDir m, MonadRandom (Performable m)) => MonadStorage t (ErgveinM t m) where
  getEncryptedPrvStorage = fmap (_storage'encryptedPrvStorage . _walletInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getEncryptedPrvStorage #-}
  getAddressByCurIx cur i = do
    currMap <- fmap (_pubStorage'currencyPubStorages . _storage'pubStorage . _walletInfo'storage) $ readExternalRef =<< asks env'authRef
    let mXPubKey = (flip (V.!?) i) . pubKeystore'external . _currencyPubStorage'pubKeystore =<< M.lookup cur currMap
    case mXPubKey of
      Nothing -> fail "NOT IMPLEMENTED" -- TODO: generate new address here
      Just (EgvPubKeyBox key _ _) ->
        let k = case key of
              ErgXPubKey k' _ -> k'
              BtcXPubKey k' _ -> k'
        in pure $ xPubExport (getCurrencyNetwork cur) k
  {-# INLINE getAddressByCurIx #-}
  getWalletName = fmap (_storage'walletName . _walletInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getWalletName #-}
  getPubStorage = fmap (_storage'pubStorage . _walletInfo'storage) $ readExternalRef =<< asks env'authRef
  {-# INLINE getPubStorage #-}
  getPubStorageD = do
    walletInfoD <- externalRefDynamic =<< asks env'authRef
    pure $ ffor walletInfoD $ \ai -> ai ^. walletInfo'storage. storage'pubStorage
  {-# INLINE getPubStorageD #-}
  storeWallet caller e = do
    ref <-  asks env'authRef
    performEvent $ ffor e $ \_ -> do
        walletInfo <- readExternalRef ref
        let storage = _walletInfo'storage walletInfo
        let eciesPubKey = _walletInfo'eciesPubKey walletInfo
        saveStorageToFile caller eciesPubKey storage
  {-# INLINE storeWallet #-}

  modifyPubStorage caller fe = do
    authRef   <- asks env'authRef
    chan      <- asks env'storeChan
    performEvent $ ffor fe $ \f -> do
      mai <- modifyExternalRefMaybe authRef $ \ai ->
        let mps' = f (ai ^. walletInfo'storage . storage'pubStorage)
        in (\a -> (a, a)) . (\ps' -> ai & walletInfo'storage . storage'pubStorage .~ ps') <$> mps'
      liftIO $ atomically $ traverse_ (writeTChan chan . (caller, )) mai
  {-# INLINE modifyPubStorage #-}
  getStoreMutex = asks env'storeMutex
  {-# INLINE getStoreMutex #-}
  getStoreChan = asks env'storeChan
  {-# INLINE getStoreChan #-}

-- | Execute action under authorized context or return the given value as result
-- if user is not authorized. Each time the login info changes and walletInfo'isUpdate flag is set to 'False'
-- (user logs out or logs in) the widget is updated.
liftAuth :: forall t m a . (MonadFrontBase t m, HasSepulca t m, PlatformNatives) => m a -> ErgveinM t m a -> m (Dynamic t a)
liftAuth ma0 ma = mdo
  mauthD <- getWalletInfoMaybe
  mauth0 <- sample . current $ mauthD
  (logoutE, logoutFire) <- newTriggerEvent
  let runAuthed :: WalletInfo -> m a
      runAuthed auth = do
        -- Get refs from Unauth context
        sepulca <- getSepulca
        settingsRef     <- getSettingsRef
        authRef         <- newExternalRef auth

        -- MonadClient refs
        urlsArchive     <- getArchivedAddrsRef
        inactiveUrls    <- getInactiveAddrsRef
        actvieAddrsRef  <- getActiveAddrsRef
        indexConmapRef  <- getActiveConnsRef
        indexStatusRef  <- getStatusConnsRef
        reqUrlNumRef    <- getRequiredUrlNumRef
        actUrlNumRef    <- getActiveUrlsNumRef
        timeoutRef      <- getRequestTimeoutRef
        iReqFire        <- getIndexReqFire
        indexSel        <- getIndexReqSelector  -- Node request selector :: NodeReqSelector t
        indexEF         <- getActivationEF

        -- Create data for Auth context
        (nReqE, nReqFire) <- newTriggerEvent
        let nodeSel = fanMap nReqE -- Node request selector :: NodeReqSelector t


        activeCursRef   <- newExternalRef mempty
        statRef         <- newExternalRef mempty
        filtersHeights  <- newExternalRef mempty
        fsyncRef        <- newExternalRef mempty
        consRef         <- newExternalRef mempty
        feesRef         <- newExternalRef mempty
        ratesRef        <- newExternalRef mempty
        storeMutex      <- liftIO $ newMVar ()
        storeChan       <- liftIO newTChanIO
        let env = Env {
                env'settings = settingsRef
              , env'sepulca = sepulca

              , env'authRef = authRef
              , env'logoutFire = logoutFire ()
              , env'activeCursRef = activeCursRef
              , env'filtersHeights = filtersHeights
              , env'statusUpdates = statRef
              , env'filtersSyncRef = fsyncRef
              , env'nodeConsRef = consRef
              , env'nodeReqSelector = nodeSel
              , env'nodeReqFire = nReqFire
              , env'feesStore = feesRef
              , env'storeMutex = storeMutex
              , env'storeChan = storeChan
              , env'ratesRef  = ratesRef

              , env'addrsArchive = urlsArchive
              , env'inactiveAddrs = inactiveUrls
              , env'activeAddrs = actvieAddrsRef
              , env'indexConmap = indexConmapRef
              , env'indexStatus = indexStatusRef
              , env'reqUrlNum = reqUrlNumRef
              , env'actUrlNum = actUrlNumRef
              , env'timeout = timeoutRef
              , env'indexReqSel = indexSel
              , env'indexReqFire = iReqFire
              , env'activateIndexEF = indexEF
              }
        runReaderT (wrapped "liftAuth" ma) env
  let
    ma0' = maybe ma0 runAuthed mauth0
    newWalletInfoE = ffilter isMauthUpdate $ updated mauthD
    redrawE = leftmost [newWalletInfoE, Nothing <$ logoutE]
  networkHold ma0' $ ffor redrawE $ maybe ma0 runAuthed

isMauthUpdate :: Maybe WalletInfo -> Bool
isMauthUpdate mauth = case mauth of
  Nothing -> True
  Just auth -> not $ _walletInfo'isUpdate auth

-- | Lift action that doesn't require authorisation in context where auth is mandatory
liftUnauthed :: m a -> ErgveinM t m a
liftUnauthed ma = ReaderT $ const ma

wrapped :: MonadFrontBase t m => Text -> ErgveinM t m a -> ErgveinM t m a
wrapped caller ma = do
  void $ storeWallet clr =<< getPostBuild
  buildE <- getPostBuild
  ac <- _pubStorage'activeCurrencies <$> getPubStorage
  void . updateActiveCurs $ fmap (\cl -> const (S.fromList cl)) $ ac <$ buildE
  ma
  where clr = caller <> ":" <> "wrapped"
