module Ergvein.Wallet.Monad.Front(
    MonadFront
  , MonadFrontBase(..)
  , MonadFrontAuth(..)
  , AuthInfo(..)
  , Password
  , NodeReqSelector
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
  , postNodeMessage
  , broadcastNodeMessage
  , requestManyFromNode
  , setFiltersSync
  , setSyncProgress
  , updateActiveCurs
  , requestRandomIndexer
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

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Network.Socket (SockAddr)
import Language.Javascript.JSaddle hiding ((!!))
import Reflex
import Reflex.Dom hiding (run, mainWidgetWithCss, textInput)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types (Message(..))
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Storage
import Ergvein.Types.Transaction
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Util

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type NodeReqSelector t = EventSelector t (Const2 Currency (Map SockAddr NodeMessage))

extractReq :: Reflex t => NodeReqSelector t -> Currency -> SockAddr -> Event t NodeMessage
extractReq sel c u = select (fanMap (select sel $ Const2 c)) $ Const2 u

-- | Authorized context. Has access to storage and indexer's functionality
type MonadFront t m = (
    MonadFrontAuth t m
  , MonadStorage t m
  , MonadIndexClient t m
  )

class MonadFrontBase t m => MonadFrontAuth t m | m -> t where
  -- | Internal method.
  getSyncProgressRef :: m (ExternalRef t (Map Currency SyncStage))
  -- | Internal method to get flag if we has fully synced filters at the moment.
  getFiltersSyncRef :: m (ExternalRef t (Map Currency Bool))
  -- | Get activeCursRef Internal
  getActiveCursRef :: m (ExternalRef t (S.Set Currency))
  -- | Internal method to get connection map ref
  getNodeConnRef  :: m (ExternalRef t (ConnMap t))
  -- | Get node request event
  getNodeNodeReqSelector :: m (NodeReqSelector t)
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

postNodeMessage :: MonadFrontAuth t m => Currency -> Event t (SockAddr, NodeMessage) -> m ()
postNodeMessage cur reqE = do
  nodeReqFire <- getNodeReqFire
  performFork_ $ ffor reqE $ \(u, msg) ->
    liftIO . nodeReqFire $ M.singleton cur $ M.singleton u msg
{-# INLINE postNodeMessage #-}

broadcastNodeMessage :: MonadFrontAuth t m => Currency -> Event t NodeMessage -> m ()
broadcastNodeMessage cur reqE = do
  nodeReqFire <- getNodeReqFire
  nodeConnRef <- getNodeConnRef
  performFork_ $ ffor reqE $ \msg -> do
    reqs <- fmap ((<$) msg . fromMaybe (M.empty) . getAllConnByCurrency cur) $ readExternalRef nodeConnRef
    liftIO . nodeReqFire $ M.singleton cur reqs
{-# INLINE broadcastNodeMessage #-}

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
getSyncProgress :: MonadFrontAuth t m => Currency -> m (Dynamic t SyncStage)
getSyncProgress cur = do
  syncMapD <- externalRefDynamic =<< getSyncProgressRef
  pure $ fmap (fromMaybe NotActive . M.lookup cur) syncMapD
{-# INLINE getSyncProgress #-}

-- | Set global sync process value each time the event is fired
setSyncProgress :: MonadFrontAuth t m => Event t (SyncProgress) -> m (Event t ())
setSyncProgress spE = do
  syncProgRef <- getSyncProgressRef
  performEvent $ ffor spE $ \(SyncProgress cur sp) -> do
    modifyExternalRef_ syncProgRef $ M.insert cur sp
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
  settingsRef <- getSettingsRef
  fmap updated $ widgetHold (pure ()) $ ffor updE $ \f -> do
    (_, _) <- modifyExternalRef curRef $ \cs -> let
      cs' = f cs
      offUrls = S.map (, False) $ S.difference cs cs'
      onUrls  = S.map (, True)  $ S.difference cs' cs
      onUrls' = S.map (, True)  $ S.intersection cs cs'
      dm = M.fromList $ S.toList $ offUrls <> onUrls <> onUrls'
      in (cs',(dm, S.toList cs'))
    settings <- readExternalRef settingsRef
    writeExternalRef settingsRef settings
    storeSettings settings
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
getCurrentHeight :: (MonadFrontAuth t m, MonadStorage t m) => Currency -> m (Dynamic t Integer)
getCurrentHeight c = do
  psD <- getPubStorageD
  pure $ ffor psD $ \ps -> fromIntegral $ fromMaybe 0 $ ps ^. pubStorage'currencyPubStorages . at c
    & \mcps -> ffor mcps $ \cps -> cps ^. currencyPubStorage'chainHeight

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

requestRandomIndexer :: MonadFront t m => Event t (Currency, Message) -> m (Event t (SockAddr, Message))
requestRandomIndexer reqE = mdo
  let actE = leftmost [Just <$> reqE, Nothing <$ sentE]
  sentE <- widgetHoldE (pure never) $ ffor actE $ \case
    Nothing -> pure never
    Just (cur, req) -> requester cur req
  pure sentE

requester :: MonadFront t m => Currency -> Message -> m (Event t (SockAddr, Message))
requester cur req = mdo
  buildE <- getPostBuild
  respD <- holdDyn True $ False <$ respE
  te <- widgetHoldDynE $ ffor respD $ \b -> if b
    then tickLossyFromPostBuildTime timeout
    else pure never
  let goE = leftmost [buildE, () <$ te]
  respE <- widgetHoldE (pure never) $ ffor goE $ const $ do
    mconn <- randomElem =<< getOpenSyncedConns cur
    case mconn of
      Nothing -> pure never
      Just conn -> do
        requestIndexerWhenOpen conn req
        pure $ (indexConAddr conn,) <$> indexConRespE conn
  pure respE
  where
    timeout = 5 -- NominalDiffTime, seconds

-- | Designed to be used inside a widgetHold, samples dynamics
getOpenSyncedConns :: MonadFront t m => Currency -> m [IndexerConnection t]
getOpenSyncedConns cur = do
  conns <- readExternalRef =<< getActiveConnsRef
  walletHeightD <- getCurrentHeight cur
  fmap catMaybes $ flip traverse (M.elems conns) $ \con -> do
    isUp <- sampleDyn $ indexConIsUp con
    walletHeight <- sampleDyn walletHeightD
    if not isUp then pure Nothing else do
      indexerHeight <- fmap (M.lookup cur) $ sampleDyn $ indexerConHeight con
      pure $ case (walletHeight, fromIntegral <$> indexerHeight) of
        (wh, Just ih) -> if wh == ih || wh - ih == 1 then Just con else Nothing
        _ -> Nothing

randomElem :: MonadIO m => [a] -> m (Maybe a)
randomElem xs = case xs of
  [] -> pure Nothing
  _ -> do
    i <- liftIO $ randomRIO (0, length xs - 1)
    pure $ Just $ xs!!i
