module Ergvein.Wallet.Worker.Node
  (
    btcNodeController
  ) where

import Control.Exception
import Control.Lens
import Control.Monad.Random
import Data.IP
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Time
import Network.DNS
import Network.Haskoin.Constants
import Network.Haskoin.Crypto
import Network.Haskoin.Network
import Network.Haskoin.Transaction
import Network.Socket
import Reflex.ExternalRef

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Derive
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Types.Transaction as ETT
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Node.BTC.Mempool
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Sync.Status
import Ergvein.Wallet.Tx
import Ergvein.Wallet.Util

import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8
import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

minNodeNum :: Int
minNodeNum = 3

firstTierNodeNum :: Int
firstTierNodeNum = 10

saStorageSize :: Int
saStorageSize = 50

btcLog :: (PlatformNatives, MonadIO m) => Text -> m ()
btcLog v = logWrite $ "[nodeController][" <> showt BTC <> "]: " <> v

btcRefrTimeout :: NominalDiffTime
btcRefrTimeout = 5

btcNodeController :: MonadFront t m => m ()
btcNodeController = mdo
  btcLog "Starting"
  buildE    <- delay 0.1 =<< getPostBuild
  sel       <- getNodeNodeReqSelector
  conMapD   <- getNodeConnectionsD
  nodeRef   <- getNodeConnRef
  te        <- fmap void $ tickLossyFromPostBuildTime btcRefrTimeout

  pubStorageD <- getPubStorageD

  let txidsD = ffor pubStorageD $ \ps -> S.fromList $ M.keys $ ps ^. pubStorage'currencyPubStorages . at BTC . non (error "btcNodeController: not exsisting store!") . currencyPubStorage'transactions

  let btcLenD = ffor conMapD $ fromMaybe 0 . fmap M.size . DM.lookup BTCTag
  let te' = poke te $ const $ do
        cm <- sample . current $ conMapD
        let nodes = fromMaybe [] $ fmap M.elems $ DM.lookup BTCTag cm
        stats <- traverse (sampleDyn . nodeconIsUp) nodes
        pure $ length $ filter id stats
  -- Get an url to connect if:
  -- 1. BTC conMap is updated
  -- 2. The first minNodeNum times an urls is added to the storage
  -- 3. Suppose 1st event fired and the storage is empty, then try again after btcRefrTimeout
  let tickE = leftmost [updated btcLenD, 0 <$ fstRunE, te']
  let urlE = flip push tickE $ \l -> if l >= minNodeNum
        then pure Nothing
        else fmap listToMaybe $ sampleDyn urlStoreD
  (urlStoreD, fstRunE) <- mkUrlBatcher sel urlE

  let (remNodeUrlE, txE) = switchTuple $ splitDynPure $ fmap (unzip . M.elems) tmpD
  let remNodeE = (`M.singleton`Nothing) <$> remNodeUrlE
  let addNodeE = (`M.singleton` Just ()) <$> urlE
  let listActionE = leftmost [addNodeE, remNodeE]

  tmpD <- listWithKeyShallowDiff M.empty listActionE $ \u _ _ -> do
    let reqE = extractReq sel BTC u
    node <- initBTCNode True u reqE
    modifyExternalRef nodeRef $ \cm -> (addNodeConn (NodeConnBTC node) cm, ())
    closeE' <- delay 0.1 $ nodeconCloseE node
    closeE <- performEvent $ ffor closeE' $ const $
      modifyExternalRef nodeRef $ \cm -> (removeNodeConn BTCTag u cm, ())
    let respE = nodeconRespE node
    let txInvsE = flip push respE $ \case
          MInv inv -> do
            txids <- sampleDyn txidsD
            pure $ filterTxInvs txids inv
          _ -> pure Nothing
        reqTxE = fmap ((u,) . NodeReqBTC . MGetData . GetData) $ txInvsE
    requestFromNode reqTxE
    let newTxE = fforMaybe respE $ \case
          MTx tx -> Just tx
          _ -> Nothing
    myTxSender u respE
    pure $ (u <$ closeE, newTxE)

  _ <- requestBTCMempool =<< delay 1 =<< getPostBuild
  void $ btcMempoolTxInserter txE
  where
    switchTuple (a, b) = (switchDyn . fmap leftmost $ a, switchDyn . fmap leftmost $ b)

myTxSender :: MonadFront t m => SockAddr -> Event t Message -> m ()
myTxSender addr msgE = do
  pubD <- getPubStorageD
  let txsD = do
        ps <- pubD
        let store = ps ^. pubStorage'currencyPubStorages . at BTC . non (error "btcNodeController: not exsisting store!")
        let txids = store ^. currencyPubStorage'outgoing
        let txmap = store ^. currencyPubStorage'transactions
        pure (txids, txmap)
  requestManyFromNode $ flip push msgE $ \case
    MGetData (GetData invs) -> fmap (Just . (addr,) . uncurry (mkTxMessages invs)) $ sampleDyn txsD
    _ -> pure Nothing
  pure ()

-- | Filter TxHashes from Inv vector. Return Nothing if no TxHashes are present
mkTxMessages :: [InvVector] -> S.Set TxId -> M.Map TxId EgvTx -> [NodeReqG]
mkTxMessages invs txids txmap = foo invs [] $ \acc iv -> case invType iv of
  InvTx -> let
    txid    = ETT.TxHash $ getHash256 $ invHash iv
    b       = S.member txid txids
    metx    = if b then M.lookup txid txmap else Nothing
    mbtctx  = join $ ffor metx $ \case
      BtcTx tx _ -> Just $ NodeReqBTC $ MTx tx
      _ -> Nothing
    in maybe acc (\t -> t:acc) mbtctx
  _ -> acc
  where
    foo ta b f = L.foldl' f b ta

-- | Filter TxHashes from Inv vector. Return Nothing if no TxHashes are present
filterTxInvs :: S.Set TxId -> Inv -> Maybe [InvVector]
filterTxInvs txids (Inv invs) = case txs of
  [] -> Nothing
  _ -> Just txs
  where
    txs = catMaybes $ ffor invs $ \iv -> case invType iv of
      InvTx -> let
        txh = ETT.TxHash $ getHash256 $ invHash iv
        b = S.member txh txids
        in if b then Nothing else Just iv
      _ -> Nothing

data SAStorageAct = SAAdd [SockAddr] | SARemove SockAddr | SAClear

handleSAStore :: SAStorageAct -> S.Set SockAddr -> Maybe (S.Set SockAddr)
handleSAStore sact acc = case sact of
  SAClear -> Just S.empty
  SAAdd sas -> let
    l = S.size acc
    ltotal = l + (length sas)
    in if l >= saStorageSize
      then Nothing
      else Just $ S.union acc $ S.fromList $ if ltotal <= saStorageSize
        then sas
        else take (ltotal - saStorageSize) sas
  SARemove sa -> Just $ S.delete sa acc


-- | Creates a dynamic storage for BTC nodes urls
-- Collects saStorageSize urls
-- Takes an event to remove an address from the storage
-- If storage is empty, requests another batch of urls of size saStorageSize
-- Returns the storage and an event which fires first minNodeNum times
-- That event allows the controller to connect to nodes immediately once there is at least 1 connection
mkUrlBatcher :: MonadFrontAuth t m
  => NodeReqSelector t -> Event t SockAddr -> m (Dynamic t [SockAddr], Event t ())
mkUrlBatcher sel remE = mdo
  buildE <- getPostBuild
  remCntD <- count remE
  let goE = leftmost [Just saStorageSize <$ buildE, actE]
  hostAddrsE <- fmap switchDyn $ widgetHold (pure never) $ ffor goE $ \case
    Nothing -> pure never
    Just n -> do
      btcLog $ "Getting a new batch: " <> showt n
      initNodesE <- getRandomBTCNodesFromDNS sel firstTierNodeNum
      fmap switchDyn $ widgetHold (pure never) $ ffor initNodesE $ \initNodes -> do
        es <- flip traverse initNodes $ \node -> do
          reqE <- fmap (NodeReqBTC MGetAddr <$) getPostBuild
          requestNodeWait node reqE
          pure $ fforMaybe (nodeconRespE node) $ \case
            MAddr (Addr nats) -> let
              addrs = snd $ unzip nats
              segwits = filter (\u -> BI.testBit (naServices u) 3) addrs
              in Just $ fmap naAddress segwits
            _ -> Nothing
        pure $ leftmost es
  sasE <- performFork $ ffor hostAddrsE $ \hs ->
    liftIO $ fmap (SAAdd . catMaybes) $ flip traverse hs $ \h ->
      catch (fmap Just $ evaluate $ hostToSockAddr h) (\(_ :: SomeException) -> pure Nothing)
  urlsD <- foldDynMaybe handleSAStore S.empty $ leftmost [sasE, SARemove <$> remE]
  -- let addr = SockAddrInet 8333 $ tupleToHostAddress (127,0,0,1)
  -- let urlsD' = S.insert addr <$> urlsD
  -- performEvent_ $ ffor (updated urlsD') $ liftIO . print
  let actE = flip push (updated urlsD) $ \acc -> if S.size acc >= saStorageSize
        then pure $ Just Nothing          -- If the storage is full, stop connections
        else if S.size acc /= 0
          then pure Nothing               -- If it's in between, do nothing
          else do                         -- If the storage is empty, request saStorageSize more
            remCnt <- sampleDyn remCntD
            if remCnt <= minNodeNum
              then pure Nothing           -- Do not fire for first minNodeNum updates
              else pure $ Just (Just saStorageSize)
  let nonNullE = fforMaybe (updated urlsD) (\urls -> if S.null urls then Nothing else Just ())
  cntD <- count nonNullE
  fstRunE <- eventToNextFrame $ fforMaybe (updated cntD) $ \c -> if c <= minNodeNum then Just () else Nothing
  pure $ (S.toList <$> urlsD, fstRunE)

-- | Connects to DNS servers, gets n urls and initializes connection to those nodes
getRandomBTCNodesFromDNS :: MonadFrontAuth t m => NodeReqSelector t -> Int -> m (Event t [NodeBTC t])
getRandomBTCNodesFromDNS sel n = do
  buildE <- getPostBuild
  let dnsUrls = getSeeds btcNetwork
  i <- liftIO $ randomRIO (0, length dnsUrls - 1)
  setSyncProgress $ (SyncProgress BTC SyncGettingNodeAddresses) <$ buildE
  rs <- mkResolvSeed
  urlsE <- performFork $ (requestNodesFromBTCDNS rs (dnsUrls!!i) n) <$ buildE
  setSyncProgress $ (SyncProgress BTC SyncConnectingToPeers) <$ urlsE
  nodesD <- widgetHold (pure []) $ ffor urlsE $ \urls -> flip traverse urls $ \u -> let
    reqE = extractReq sel BTC u
    in initBTCNode False u reqE
  pure $ fforMaybe (updated nodesD) $ \case
    [] -> Nothing
    ns -> Just ns

-- | Connects to DNS servers and collects n BTC node addresses
requestNodesFromBTCDNS :: (MonadIO m, PlatformNatives) => ResolvSeed -> String -> Int -> m [SockAddr]
requestNodesFromBTCDNS rs dnsurl n = liftIO $ do
  res <- fmap (either (const []) id) $ withResolver rs $ \resolver -> lookupA resolver $ B8.pack dnsurl
  urls <- randomVals n res
  pure $ ffor urls $ \u -> let
    h = toHostAddress u
    p = fromIntegral $ getDefaultPort btcNetwork
    in SockAddrInet p h

-- | Pick n random values from a list
randomVals :: MonadIO m => Int -> [a] -> m [a]
randomVals l urls = if l >= n
  then pure urls
  else (fmap . fmap) (\i -> urls!!i) $ mkIndexes []
  where
    n = length urls
    mkIndexes :: MonadIO m => [Int] -> m [Int]
    mkIndexes acc = do
      i <- liftIO $ randomRIO (0, n - 1)
      if i `elem` acc
        then mkIndexes acc
        else let acc' = i:acc in if length acc' == l
          then pure acc'
          else mkIndexes acc'
