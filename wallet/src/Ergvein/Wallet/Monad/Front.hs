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
  , getBtcNodesD
  , getErgoNodesD
  , getWalletStatus
  , requestBroadcast
  , sendRandomNode
  , requestFromNode
  , postNodeMessage
  , broadcastNodeMessage
  , requestManyFromNode
  , setFiltersSync
  , updateWalletStatusNormal
  , updateWalletStatusRestore
  , updateActiveCurs
  , requestRandomIndexer
  , getRateByFiatD
  -- * Reexports
  , Text
  , MonadJSM
  , traverse_
  , for_
  , module Ergvein.Wallet.Monad.Prim
  , module Control.Monad
  , module Ergvein.Wallet.Monad.Base
  , module Reflex.Dom
  , module Reflex.Dom.Retractable.Class
  , module Reflex.Flunky
  , module Reflex.Network
  , module Ergvein.Wallet.Monad.Client
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Fixed
import Data.Foldable (traverse_, for_)
import Data.Functor (void)
import Data.Functor.Misc (Const2(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Language.Javascript.JSaddle hiding ((!!))
import Network.Socket (SockAddr)
import Reflex
import Reflex.Flunky
import Reflex.Network
import Reflex.Dom hiding (run, mainWidgetWithCss, textInput, askEvents)
import Reflex.Dom.Retractable.Class
import Reflex.ExternalRef

import Ergvein.Index.Protocol.Types (Message(..))
import Ergvein.Node.Constants
import Ergvein.Text
import Ergvein.Types.AuthInfo
import Ergvein.Types.Currency
import Ergvein.Types.Fees
import Ergvein.Types.Storage
import Sepulcas.Alert
import Ergvein.Wallet.Localization.Client
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Base
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Storage
import Sepulcas.Native
import Ergvein.Wallet.Node.Prim
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Settings
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Util
import Ergvein.Node.Resolve

import qualified Data.Dependent.Map as DM
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
  getWalletStatusRef :: m (ExternalRef t (Map Currency WalletStatus))
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
  -- | Get rates (e.g. BTC/USDT) ref
  getRatesRef :: m (ExternalRef t (Map Currency (Map Fiat Centi)))

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
  (fmap . fmap) (fromMaybe (M.empty) . getAllConnByCurrency cur) getNodeConnectionsD
{-# INLINE getNodesByCurrencyD #-}

-- | Get BTC nodes
getBtcNodesD :: MonadFrontAuth t m => m (Dynamic t (Map SockAddr (NodeBTC t)))
getBtcNodesD =
  (fmap . fmap) (fromMaybe (M.empty) . DM.lookup BTCTag) getNodeConnectionsD
{-# INLINE getBtcNodesD #-}

-- | Get ERGO nodes
getErgoNodesD :: MonadFrontAuth t m => m (Dynamic t (Map SockAddr (NodeERG t)))
getErgoNodesD =
  (fmap . fmap) (fromMaybe (M.empty) . DM.lookup ERGOTag) getNodeConnectionsD
{-# INLINE getErgoNodesD #-}

-- | Send a request to a specific URL
-- It's up to the caller to ensure that the URL actually points to a correct currency node
requestFromNode :: MonadFrontAuth t m => Event t (SockAddr, NodeReqG) -> m (Event t ())
requestFromNode reqE = do
  nodeReqFire <- getNodeReqFire
  performFork $ ffor reqE $ \(u, req) ->
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
  performFork_ $ ffor reqE $ \(u, reqs) -> for_ reqs $ \req ->
    let cur = getNodeReqCurrency req
    in liftIO . nodeReqFire $ M.singleton cur $ M.singleton u $ NodeMsgReq req
{-# INLINE requestManyFromNode #-}

-- | Get global wallet status value
getWalletStatus :: MonadFrontAuth t m => Currency -> m (Dynamic t WalletStatus)
getWalletStatus cur = do
  statMapD <- externalRefDynamic =<< getWalletStatusRef
  pure $ fmap (fromMaybe emptyWalletStatus . M.lookup cur) statMapD
{-# INLINE getWalletStatus #-}

-- | Updates normal wallet status each time the event is fired
updateWalletStatusNormal :: MonadFrontAuth t m => Currency -> Event t (WalletStatusNormal -> WalletStatusNormal) -> m (Event t ())
updateWalletStatusNormal cur updateE = do
  walletStatusRef <- getWalletStatusRef
  performEvent $ ffor updateE $ \f -> do
    modifyExternalRef_ walletStatusRef $ M.insertWith (\_ old -> updateStatus f old) cur (updateStatus f emptyWalletStatus)
  where
    updateStatus g s@WalletStatus{..} = s {walletStatus'normal = g walletStatus'normal}

updateWalletStatusRestore :: MonadFrontAuth t m => Currency -> Event t (WalletStatusRestore -> WalletStatusRestore) -> m (Event t ())
updateWalletStatusRestore cur updateE = do
  walletStatusRef <- getWalletStatusRef
  performEvent $ ffor updateE $ \f -> do
    modifyExternalRef_ walletStatusRef $ M.insertWith (\_ old -> updateStatus f old) cur (updateStatus f emptyWalletStatus)
  where
    updateStatus g s@WalletStatus{..} = s {walletStatus'restore = g walletStatus'restore}

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
  fmap updated $ networkHold (pure ()) $ ffor updE $ \f -> do
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

-- | Send message to random crypto node
sendRandomNode :: MonadFrontAuth t m => Event t NodeReqG -> m (Event t ())
sendRandomNode reqE = do
  nodeReqFire <- getNodeReqFire
  nodeConnRef <- getNodeConnRef
  performFork $ ffor reqE $ \req -> do
    let cur = getNodeReqCurrency req
    nodes <- fmap (maybe [] M.toList . getAllConnByCurrency cur) $ readExternalRef nodeConnRef
    mnode <- randomElem nodes
    for_ mnode $ \(addr, _) ->
      liftIO . nodeReqFire . M.singleton cur . M.singleton addr . NodeMsgReq $ req

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

requestRandomIndexer :: MonadFront t m => Event t (Currency, Message) -> m (Event t (ErgveinNodeAddr, Message))
requestRandomIndexer reqE = mdo
  let actE = leftmost [Just <$> reqE, Nothing <$ sentE]
  sentE <- networkHoldE (pure never) $ ffor actE $ \case
    Nothing -> pure never
    Just (cur, req) -> requester cur req
  pure sentE

requester :: MonadFront t m => Currency -> Message -> m (Event t (ErgveinNodeAddr, Message))
requester cur req = mdo
  buildE <- getPostBuild
  respD <- holdDyn True $ False <$ respE
  te <- networkHoldDynE $ ffor respD $ \b -> if b
    then tickLossyFromPostBuildTime timeout
    else pure never
  let goE = leftmost [buildE, () <$ te]
  respE <- networkHoldE (pure never) $ ffor goE $ const $ do
    conns <- getOpenSyncedConns cur
    logWrite $ "Has " <> showt (length conns) <> " synced connections to indexers"
    mconn <- randomElem conns
    case mconn of
      Nothing -> do
        logWrite "Cannot select indexer for request"
        buildE' <- delay 0.1 =<< getPostBuild
        showWarnMsg $ ffor buildE' $ const CMSAllOutOfSync
        pure never
      Just conn -> do
        logWrite $ "Selected indexer " <> showt (indexConAddr conn)
        void $ requestIndexerWhenOpen conn req
        pure $ (indexConName conn,) <$> indexConRespE conn
  pure respE
  where
    timeout = 5 -- NominalDiffTime, seconds

-- | Designed to be used inside a networkHold, samples dynamics
getOpenSyncedConns :: MonadFront t m => Currency -> m [IndexerConnection t]
getOpenSyncedConns cur = do
  conns <- readExternalRef =<< getActiveConnsRef
  logWrite $ "Has " <> showt (length conns) <> " active connections to indexers"
  walletHeightD <- getCurrentHeight cur
  fmap catMaybes $ flip traverse (M.elems conns) $ \con -> do
    isUp <- sampleDyn $ indexConIsUp con
    logWrite $ "Connection " <> showt (indexConAddr con) <> " is up: " <> showt isUp
    walletHeight <- sampleDyn walletHeightD
    if not isUp then pure Nothing else do
      indexerHeight <- fmap (M.lookup cur) $ sampleDyn $ indexConHeight con
      logWrite $ "Wallet height " <> showt walletHeight
      logWrite $ "Indexer height " <> showt indexerHeight
      pure $ case (walletHeight, fromIntegral <$> indexerHeight) of
        (wh, Just ih) -> if wh <= ih || wh - ih == 1 then Just con else Nothing
        _ -> Nothing

randomElem :: MonadIO m => [a] -> m (Maybe a)
randomElem xs = case xs of
  [] -> pure Nothing
  _ -> do
    i <- liftIO $ randomRIO (0, length xs - 1)
    pure $ Just $ xs!!i

getRateByFiatD :: MonadFront t m => Currency -> Fiat -> m (Dynamic t (Maybe Centi))
getRateByFiatD c f = do
  ratesD <- externalRefDynamic =<< getRatesRef
  pure $ ffor ratesD $ join . fmap (M.lookup f ) . M.lookup c
