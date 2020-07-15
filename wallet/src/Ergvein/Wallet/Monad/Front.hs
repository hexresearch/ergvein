module Ergvein.Wallet.Monad.Front(
    MonadFront
  , MonadFrontBase(..)
  , MonadFrontAuth(..)
  , AuthInfo(..)
  , Password
  , RequestSelector
  -- * Helpers
  , extractReq
  , getActiveCursD
  , getAuthInfo
  , getCurrentHeight
  , getFeesD
  , getFiltersSync
  , getLoginD
  , getNodeConnectionsD
  , getNodesByCurrencyD
  , getSyncProgress
  , requestBroadcast
  , requestFromNode
  , requestManyFromNode
  , setCurrentHeight
  , setFiltersSync
  , setSyncProgress
  , updateActiveCurs
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , module Ergvein.Wallet.Monad.Prim
  , module Ergvein.Wallet.Monad.Base
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Control.Monad
  , module Ergvein.Wallet.Monad.Client
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Socket (SockAddr)
import Language.Javascript.JSaddle
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss, textInput)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Storage
import Ergvein.Wallet.Filters.Storage
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Status

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type RequestSelector t = EventSelector t (Const2 Currency (Map SockAddr NodeMessage))

extractReq :: Reflex t => RequestSelector t -> Currency -> SockAddr -> Event t NodeMessage
extractReq sel c u = select (fanMap (select sel $ Const2 c)) $ Const2 u

-- | Authorized context. Has access to storage and indexer's functionality
type MonadFront t m = (
    MonadFrontAuth t m
  , MonadStorage t m
  , MonadClient t m
  , HasFiltersStorage t m
  , HasFiltersStorage t (Performable m)
  )

class MonadFrontBase t m => MonadFrontAuth t m | m -> t where
  -- | Internal method.
  getSyncProgressRef :: m (ExternalRef t SyncProgress)
  -- | Internal method to get reference with known heights per currency.
  getHeightRef :: m (ExternalRef t (Map Currency Integer))
  -- | Internal method to get flag if we has fully synced filters at the moment.
  getFiltersSyncRef :: m (ExternalRef t (Map Currency Bool))
  -- | Get activeCursRef Internal
  getActiveCursRef :: m (ExternalRef t (S.Set Currency))
  -- | Internal method to get connection map ref
  getNodeConnRef  :: m (ExternalRef t (ConnMap t))
  -- | Get node request event
  getNodeRequestSelector :: m (RequestSelector t)
  -- | Get fees ref. Internal
  getFeesRef :: m (ExternalRef t (Map Currency FeeBundle))
  -- | Get node request trigger
  getNodeReqFire :: m (Map Currency (Map SockAddr NodeMessage) -> IO ())
  -- | Get authed info
  getAuthInfoRef :: m (ExternalRef t AuthInfo)


-- | Get connections map
getNodeConnectionsD :: MonadFrontAuth t m => m (Dynamic t (ConnMap t))
getNodeConnectionsD = externalRefDynamic =<< getNodeConnRef
{-# INLINE getNodeConnectionsD #-}

-- | Get the login. Convenience function
getLoginD :: MonadFrontAuth t m => m (Dynamic t Text)
getLoginD = (fmap . fmap) _authInfo'login . externalRefDynamic =<< getAuthInfoRef
{-# INLINE getLoginD #-}

-- | Get nodes by currency. Basically useless, but who knows
getNodesByCurrencyD :: MonadFrontAuth t m => Currency -> m (Dynamic t (Map SockAddr (NodeConn t)))
getNodesByCurrencyD cur =
  (fmap . fmap) (fromMaybe (M.empty) . getAllConnByCurrency cur) . externalRefDynamic =<< getNodeConnRef
{-# INLINE getNodesByCurrencyD #-}

-- | Send a request to a specific URL
-- It's up to the caller to ensure that the URL actually points to a correct currency node
requestFromNode :: MonadFrontAuth t m => Event t (SockAddr, NodeReqG) -> m ()
requestFromNode reqE = do
  nodeReqFire <- getNodeReqFire
  performFork_ $ ffor reqE $ \(u, req) ->
    let cur = getNodeReqCurrency req
    in liftIO . nodeReqFire $ M.singleton cur $ M.singleton u $ NodeMsgReq req
{-# INLINE requestFromNode #-}

-- | Send a multiple requests a specific URL
-- It's up to the caller to ensure that the URL actually points to a correct currency node
requestManyFromNode :: MonadFrontAuth t m => Event t (SockAddr, [NodeReqG]) -> m ()
requestManyFromNode reqE = do
  nodeReqFire <- getNodeReqFire
  performFork_ $ ffor reqE $ \(u, reqs) -> flip traverse_ reqs $ \req ->
    let cur = getNodeReqCurrency req
    in liftIO . nodeReqFire $ M.singleton cur $ M.singleton u $ NodeMsgReq req
{-# INLINE requestManyFromNode #-}

-- | Get global sync process value
getSyncProgress :: MonadFrontAuth t m => m (Dynamic t SyncProgress)
getSyncProgress = externalRefDynamic =<< getSyncProgressRef
{-# INLINE getSyncProgress #-}

-- | Set global sync process value each time the event is fired
setSyncProgress :: MonadFrontAuth t m => Event t SyncProgress -> m ()
setSyncProgress spE = do
  syncProgRef <- getSyncProgressRef
  syncRef <- getFiltersSyncRef
  performEvent_ $ ffor spE $ \sp -> do
    writeExternalRef syncProgRef sp
    case sp of
      SyncMeta cur SyncFilters a t -> when (a >= t && t /= 0) $
        modifyExternalRef syncRef $ \m -> (,()) $ M.insert cur True m
      _ -> pure ()
{-# INLINE setSyncProgress #-}

-- | Get auth info. Not a Maybe since this is authorized context
getAuthInfo :: MonadFrontAuth t m => m (Dynamic t AuthInfo)
getAuthInfo = externalRefDynamic =<< getAuthInfoRef

-- | Get activeCursRef Internal
getActiveCursD :: MonadFrontAuth t m => m (Dynamic t (S.Set Currency))
getActiveCursD = externalRefDynamic =<< getActiveCursRef

-- | Update active currencies
-- TODO: This one clearly does nothing. Fix it sometime
updateActiveCurs :: MonadFrontAuth t m => Event t (S.Set Currency -> S.Set Currency) -> m (Event t ())
updateActiveCurs updE = do
  curRef      <- getActiveCursRef
  nodeRef     <- getNodeConnRef
  settingsRef <- getSettingsRef
  authRef     <- getAuthInfoRef
  sel         <- getNodeRequestSelector
  fmap updated $ widgetHold (pure ()) $ ffor updE $ \f -> do
    (diffMap, newcs) <- modifyExternalRef curRef $ \cs -> let
      cs' = f cs
      offUrls = S.map (, False) $ S.difference cs cs'
      onUrls  = S.map (, True)  $ S.difference cs' cs
      onUrls' = S.map (, True)  $ S.intersection cs cs'
      dm = M.fromList $ S.toList $ offUrls <> onUrls <> onUrls'
      in (cs',(dm, S.toList cs'))
    settings <- readExternalRef settingsRef
    login    <- fmap _authInfo'login $ readExternalRef authRef
    let urls = settingsNodes settings
        set' = settings

    writeExternalRef settingsRef set'
    storeSettings set'
    pure ()
{-# INLINE updateActiveCurs #-}

-- | Send the same requests to all URLs
requestBroadcast :: MonadFrontAuth t m => Event t NodeReqG -> m (Event t ())
requestBroadcast reqE = do
  nodeReqFire <- getNodeReqFire
  nodeConnRef <- getNodeConnRef
  performFork $ ffor reqE $ \req -> do
    let cur = getNodeReqCurrency req
    reqs <- fmap ((<$) (NodeMsgReq req) . fromMaybe (M.empty) . getAllConnByCurrency cur) $ readExternalRef nodeConnRef
    liftIO . nodeReqFire $ M.singleton cur reqs

-- | Get fees dynamic
getFeesD :: MonadFrontAuth t m => m (Dynamic t (Map Currency FeeBundle))
getFeesD = externalRefDynamic =<< getFeesRef

-- | Get current value of longest chain height for given currency.
getCurrentHeight :: MonadFrontAuth t m => Currency -> m (Dynamic t Integer)
getCurrentHeight c = do
  r <- getHeightRef
  md <- externalRefDynamic r
  holdUniqDyn $ (fromMaybe 0 . M.lookup c) <$> md

-- | Update current height of longest chain for given currency.
setCurrentHeight :: MonadFront t m => Currency -> Event t Integer -> m (Event t ())
setCurrentHeight c e = do
  r <- getHeightRef
  restoredD <- fmap _pubStorage'restoring <$> getPubStorageD
  setLastSeenHeight "setCurrentHeight" c $ fromIntegral <$> e
  mE <- performFork $ ffor e $ \h -> do
    h0 <- fromMaybe 0 . M.lookup c <$> readExternalRef r
    restored <- sample . current $ restoredD
    modifyExternalRef r ((, ()) . M.insert c h)
    pure $ if (h0 == 0 && not restored) then Just (c, fromIntegral (h-1)) else Nothing
  writeWalletsScannedHeight "setCurrentHeight" $ fmapMaybe id mE

-- | Get current value that tells you whether filters are fully in sync now or not
getFiltersSync :: MonadFrontAuth t m => Currency -> m (Dynamic t Bool)
getFiltersSync c = do
  d <- externalRefDynamic =<< getFiltersSyncRef
  pure $ fromMaybe False . M.lookup c <$> d

-- | Set current value that tells you whether filters are fully in sync now or not
setFiltersSync :: MonadFrontAuth t m => Currency -> Bool -> m ()
setFiltersSync c v = do
  r <- getFiltersSyncRef
  modifyExternalRef r $ (, ()) . M.insert c v
