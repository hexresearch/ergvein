module Ergvein.Wallet.Monad.Client (
    MonadIndexClient(..)
  , IndexerConnection(..)
  , IndexerMsg(..)
  , IndexReqSelector
  , getArchivedUrlsD
  , getInactiveUrlsD
  , activateURL
  , deactivateURL
  , forgetURL
  , broadcastIndexerMessage
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Time(NominalDiffTime, getCurrentTime, diffUTCTime)
import Network.HTTP.Client hiding (Proxy)
import Reflex
import Reflex.ExternalRef

import Network.Socket (SockAddr)
import Data.Functor.Misc (Const2(..))

import Ergvein.Index.Protocol.Types (Message, ScanBlock)
import Ergvein.Index.Client
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Settings

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

data IndexerConnection t = IndexerConnection {
  indexConAddr :: !SockAddr
, indexConClosedE :: !(Event t ())
, indexConOpensE :: !(Event t ())
, indexConIsUp :: !(Dynamic t Bool)
, indexConRespE :: !(Event t Message)
}

data IndexerMsg = IndexerClose | IndexerRestart | IndexerMsg Message

type IndexReqSelector t = EventSelector t (Const2 SockAddr IndexerMsg)

-- ===========================================================================
--    Monad Client. Implements all required things for client operations
-- ===========================================================================

class MonadBaseConstr t m => MonadIndexClient t m | m -> t where
  -- | Get passive urls' reference. Internal
  getArchivedUrlsRef :: m (ExternalRef t (Set SockAddr))
  -- | Internal method to get reference to indexers
  getActiveConnsRef :: m (ExternalRef t (Map SockAddr (IndexerConnection t)))
  -- | Get deactivated urls' reference. Internal
  getInactiveUrlsRef :: m (ExternalRef t (Set SockAddr))
  -- | Get reference to the minimal number of active urls. Internal
  getActiveUrlsNumRef :: m (ExternalRef t Int)
  -- | Get num reference. Internal
  getRequiredUrlNumRef :: m (ExternalRef t (Int, Int))
  -- | Get request timeout ref
  getRequestTimeoutRef :: m (ExternalRef t NominalDiffTime)
  -- | Get indexer request event
  getIndexReqSelector :: m (IndexReqSelector t)
  -- | Get indexer request trigger
  getIndexReqFire :: m (Map SockAddr IndexerMsg -> IO ())
  -- | Get activation event and trigger
  getActivationEF :: m (Event t SockAddr, SockAddr -> IO ())

-- | Get deactivated urls dynamic
getArchivedUrlsD :: MonadIndexClient t m => m (Dynamic t (Set SockAddr))
getArchivedUrlsD = externalRefDynamic =<< getArchivedUrlsRef
-- | Get deactivated urls dynamic
getInactiveUrlsD :: MonadIndexClient t m => m (Dynamic t (Set SockAddr))
getInactiveUrlsD = externalRefDynamic =<< getInactiveUrlsRef

-- | Activate an URL
activateURL :: (MonadIndexClient t m, MonadHasSettings t m) => Event t SockAddr -> m (Event t ())
activateURL addrE = do
  (_, f)    <- getActivationEF
  iaRef     <- getInactiveUrlsRef
  acrhRef   <- getArchivedUrlsRef
  setRef    <- getSettingsRef
  connsRef  <- getActiveConnsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- fmap M.keys $ readExternalRef connsRef
    f url
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveSockAddrs  = L.nub $ url:acs
        , settingsDeactivSockAddrs = ias
        , settingsPassiveSockAddrs = ars
        }
      in (s', s')
    storeSettings s
    fire ()

-- | Deactivate an URL
deactivateURL :: (MonadIndexClient t m, MonadHasSettings t m) => Event t SockAddr -> m (Event t ())
deactivateURL addrE = do
  req       <- getIndexReqFire
  iaRef     <- getInactiveUrlsRef
  setRef    <- getSettingsRef
  connsRef  <- getActiveConnsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    acs <- fmap M.keys $ readExternalRef connsRef
    ias <- modifyExternalRef iaRef  $ \us ->
      let us' = S.insert url us in (us', S.toList us')

    req $ M.singleton url IndexerClose
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveSockAddrs  = L.filter (/= url) acs
        , settingsDeactivSockAddrs = ias
        }
      in (s', s')
    storeSettings s
    fire ()

-- | Forget an url
forgetURL :: (MonadIndexClient t m, MonadHasSettings t m) => Event t SockAddr -> m (Event t ())
forgetURL addrE = do
  req       <- getIndexReqFire
  iaRef     <- getInactiveUrlsRef
  acrhRef   <- getArchivedUrlsRef
  setRef    <- getSettingsRef
  connsRef  <- getActiveConnsRef
  performEventAsync $ ffor addrE $ \url fire -> void $ liftIO $ forkOnOther $ do
    ias <- modifyExternalRef iaRef $ \us ->
      let us' = S.delete url us in (us', S.toList us')
    ars <- modifyExternalRef acrhRef $ \as ->
      let as' = S.delete url as in  (as', S.toList as')
    acs <- fmap M.keys $ readExternalRef connsRef
    req $ M.singleton url IndexerClose
    s <- modifyExternalRef setRef $ \s -> let
      s' = s {
          settingsActiveSockAddrs  = L.filter (/= url) acs
        , settingsDeactivSockAddrs = ias
        , settingsPassiveSockAddrs = ars
        }
      in (s', s')
    storeSettings s
    fire ()

broadcastIndexerMessage :: (MonadIndexClient t m) => Event t IndexerMsg -> m ()
broadcastIndexerMessage reqE = do
  connsRef  <- getActiveConnsRef
  fire <- getIndexReqFire
  performEvent_ $ ffor reqE $ \req -> do
    cm <- readExternalRef connsRef
    liftIO $ fire $ req <$ cm
