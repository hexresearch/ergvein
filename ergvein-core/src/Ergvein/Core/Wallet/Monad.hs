module Ergvein.Core.Wallet.Monad(
    MonadPreWalletConstr
  , MonadPreWallet(..)
  , getWalletInfoMaybe
  , isInsideWallet
  , MonadWalletConstr
  , MonadWallet(..)
  , getLoginD
  , getActiveCursD
  , updateActiveCurs
  , requestRandomIndexer
  , getFeesD
  , getCurrentHeight
  , getFiltersSync
  , setFiltersSync
  , getOpenSyncedConns
  , getRateByFiatD
  , module Reflex
  , module Control.Monad.IO.Class
  , module Ergvein.Core.Client.Monad
  , module Ergvein.Core.Settings.Monad
  , module Ergvein.Core.Store.Monad
  ) where

import Control.Concurrent
import Control.Lens
import Control.Monad (join)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.Fixed
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import Ergvein.Core.Client.Monad
import Ergvein.Core.Node.Monad
import Ergvein.Core.Settings.Monad
import Ergvein.Core.Store.Monad
import Ergvein.Index.Protocol.Types (Message(..))
import Ergvein.Node.Constants
import Ergvein.Text
import Ergvein.Types
import Network.Socket (SockAddr)
import Reflex
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Localize
import Reflex.Network
import Sepulcas.Alert
import Sepulcas.Log.Monad
import Sepulcas.Native

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type MonadPreWalletConstr t (m :: * -> *) = (
    MonadHold t m
  , MonadIO m
  , Reflex t
  , TriggerEvent t m
  , PerformEvent t m
  , MonadIO (Performable m)
  , Adjustable t m
  , PlatformNatives
  )

-- | Context where we can check whether we opened a wallet or not.
--
-- When user opens application, it loads monad that implements `MonadPreWallet`.
-- Next, when user opens a wallet (via entering decryption password), the new
-- monad is executed that implements `MonadPreWallet` and `MonadWallet`.
class MonadPreWalletConstr t m => MonadPreWallet t (m :: * -> *) | m -> t where
  -- | Internal method to get storage of auth info
  getWalletInfoMaybeRef :: m (ExternalRef t (Maybe WalletInfo))
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setWalletInfo :: Event t (Maybe WalletInfo) -> m (Event t ())
  -- | Manually set authorisation information for context. Used by widgets that
  -- implement actual login/logout. Some implementations may ingore 'Nothing'
  -- values if their semantic require persistent authorisation.
  setWalletInfoNow :: Proxy m -> Maybe WalletInfo -> Performable m ()
  -- | Get mutex that must be taken for synchronize write access to wallet info.
  getWalletInfoMutex :: m (MVar ())

-- | Return flag that comes 'True' as soon as user opens wallet
isInsideWallet :: MonadPreWallet t m => m (Dynamic t Bool)
isInsideWallet = (fmap . fmap) isJust getWalletInfoMaybe
{-# INLINE isInsideWallet #-}

-- | Get authorization information that can be updated if user logs or logouts
getWalletInfoMaybe :: MonadPreWallet t m => m (Dynamic t (Maybe WalletInfo))
getWalletInfoMaybe = externalRefDynamic =<< getWalletInfoMaybeRef
{-# INLINE getWalletInfoMaybe #-}

type MonadWalletConstr t (m :: * -> *) = (
    MonadHold t m
  , Adjustable t m
  , MonadFix m
  , MonadIO (Performable m)
  , MonadIO m
  , MonadLocalized t m
  , PerformEvent t m
  , PlatformNatives
  , PostBuild t m
  , Reflex t
  , TriggerEvent t m
  , MonadAlertPoster t m
  , MonadNativeLogger t m
  , LocalizedPrint ClientMessage
  , MonadClient t m
  , MonadStorage t m
  , MonadPreWallet t m
  )

class MonadWalletConstr t m => MonadWallet t m | m -> t where
  -- | Internal method to get flag if we has fully synced filters at the moment.
  getFiltersSyncRef :: m (ExternalRef t (Map Currency Bool))
  -- | Get activeCursRef Internal
  getActiveCursRef :: m (ExternalRef t (S.Set Currency))
  -- | Get fees ref. Internal
  getFeesRef :: m (ExternalRef t (Map Currency FeeBundle))
  -- | Get auth info. Not a Maybe since this is authorized context
  getWalletInfo :: m (Dynamic t WalletInfo)
  -- | Get rates (e.g. BTC/USDT) ref
  getRatesRef :: m (ExternalRef t (Map Currency (Map Fiat Centi)))

-- | Get the login. Convenience function
getLoginD :: MonadWallet t m => m (Dynamic t Text)
getLoginD = (fmap . fmap) _walletInfo'login getWalletInfo
{-# INLINE getLoginD #-}

-- | Get activeCursRef Internal
getActiveCursD :: MonadWallet t m => m (Dynamic t (S.Set Currency))
getActiveCursD = externalRefDynamic =<< getActiveCursRef

-- | Update active currencies
-- TODO: This one clearly does nothing. Fix it sometime
updateActiveCurs :: (MonadWallet t m, MonadSettings t m) => Event t (S.Set Currency -> S.Set Currency) -> m (Event t ())
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


-- | Get fees dynamic
getFeesD :: MonadWallet t m => m (Dynamic t (Map Currency FeeBundle))
getFeesD = externalRefDynamic =<< getFeesRef

-- | Get current value of longest chain height for given currency.
getCurrentHeight :: (MonadWallet t m, MonadStorage t m, MonadNode t m) => Currency -> m (Dynamic t BlockHeight)
getCurrentHeight c = do
  psD <- getPubStorageD
  startHeightD :: Dynamic t BlockHeight <- case c of
    BTC -> fmap (maybe 0 fromIntegral) <$> getNodeHeightBtc
    _ -> pure 0
  pure $ do
    ps <- psD
    h0 <- startHeightD
    let psHeight :: BlockHeight = fromMaybe 0 $ ps ^. pubStorage'currencyPubStorages . at c
          & \mcps -> ffor mcps $ \cps -> cps ^. currencyPubStorage'chainHeight
    pure $ fromIntegral $ max h0 psHeight

-- | Get current value that tells you whether filters are fully in sync now or not
getFiltersSync :: MonadWallet t m => Currency -> m (Dynamic t Bool)
getFiltersSync c = do
  d <- externalRefDynamic =<< getFiltersSyncRef
  pure $ fromMaybe False . M.lookup c <$> d

-- | Set current value that tells you whether filters are fully in sync now or not
setFiltersSync :: MonadWallet t m => Currency -> Bool -> m ()
setFiltersSync c v = do
  r <- getFiltersSyncRef
  modifyExternalRef r $ (, ()) . M.insert c v

requestRandomIndexer :: (MonadWallet t m, MonadNode t m) => Event t (Currency, Message) -> m (Event t (ErgveinNodeAddr, Message))
requestRandomIndexer reqE = mdo
  let actE = leftmost [Just <$> reqE, Nothing <$ sentE]
  sentE <- networkHoldE (pure never) $ ffor actE $ \case
    Nothing -> pure never
    Just (cur, req) -> requester cur req
  pure sentE

requester :: (MonadWallet t m, MonadNode t m) => Currency -> Message -> m (Event t (ErgveinNodeAddr, Message))
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
getOpenSyncedConns :: (MonadWallet t m, MonadNode t m) => Currency -> m [IndexerConnection t]
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

getRateByFiatD :: MonadWallet t m => Currency -> Fiat -> m (Dynamic t (Maybe Centi))
getRateByFiatD c f = do
  ratesD <- externalRefDynamic =<< getRatesRef
  pure $ ffor ratesD $ join . fmap (M.lookup f ) . M.lookup c

randomElem :: MonadIO m => [a] -> m (Maybe a)
randomElem xs = case xs of
  [] -> pure Nothing
  _ -> do
    i <- liftIO $ randomRIO (0, length xs - 1)
    pure $ Just $ xs!!i
