module Ergvein.Core.Worker.Node
  (
    btcNodeController
  ) where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Random
import Data.Either (fromRight)
import Data.IP
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Time
import Data.Traversable (for)
import Ergvein.Core.Node
import Ergvein.Core.Node.Btc.Mempool
import Ergvein.Core.Platform
import Ergvein.Core.Resolve
import Ergvein.Core.Status
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Transaction as ETT
import Network.DNS
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Haskoin.Transaction
import Network.Socket
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Fork
import Reflex.Main.Thread
import Reflex.Network
import Sepulcas.Native

import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8
import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.Haskoin.Transaction as HT

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

btcNodeController :: (MonadNode t m, MonadStorage t m, MonadSettings t m
  , MonadWallet t m, MonadStatus t m, MonadHasMain m) => m ()
btcNodeController = mdo
  btcLog "Starting"
  sel       <- getNodeNodeReqSelector
  conMapD   <- getNodeConnectionsD
  nodeRef   <- getNodeConnRef
  te        <- void <$> tickLossyFromPostBuildTime btcRefrTimeout

  pubStorageD <- getPubStorageD

  let txidsD = ffor pubStorageD $ \ps -> M.keysSet $ ps ^. btcPubStorage . currencyPubStorage'transactions

  let btcLenD = ffor conMapD $ maybe 0 M.size . DM.lookup BtcTag
  let te' = poke te $ const $ do
        cm <- sample . current $ conMapD
        let nodes = maybe [] M.elems (DM.lookup BtcTag cm)
        stats <- traverse (sampleDyn . nodeconIsUp) nodes
        pure $ length $ filter id stats
  -- Get an url to connect if:
  -- 1. BTC conMap is updated
  -- 2. The first minNodeNum times an urls is added to the storage
  -- 3. Suppose 1st event fired and the storage is empty, then try again after btcRefrTimeout
  let tickE = leftmost [updated btcLenD, 0 <$ fstRunE, te']
  let urlE = flip push tickE $ \l -> if l >= minNodeNum
        then pure Nothing
        else listToMaybe <$> sampleDyn urlStoreD
  (urlStoreD, fstRunE) <- mkUrlBatcher sel urlE

  let (remNodeUrlE, txE) = switchTuple $ splitDynPure $ fmap (unzip . M.elems) tmpD
  let remNodeE = (`M.singleton` Nothing) <$> remNodeUrlE
  let addNodeE = (`M.singleton` Just ()) <$> urlE
  let listActionE = leftmost [addNodeE, remNodeE]

  tmpD <- listWithKeyShallowDiff M.empty listActionE $ \u _ _ -> do
    let reqE = extractReq sel BTC u
    node <- initBtcNode True u reqE
    modifyExternalRef nodeRef $ \cm -> (addNodeConn (NodeConnBtc node) cm, ())
    closeE' <- delay 0.1 $ nodeconCloseE node
    closeE <- performEvent $ ffor closeE' $ const $
      modifyExternalRef nodeRef $ \cm -> (removeNodeConn BtcTag u cm, ())
    let respE = nodeconRespE node
    let txInvsE = flip push respE $ \case
          MInv inv -> do
            txids <- sampleDyn txidsD
            pure $ filterTxInvs txids inv
          _ -> pure Nothing
        reqTxE = (u,) . NodeReqBtc . MGetData . GetData <$> txInvsE
    _ <- requestFromNode reqTxE
    let newTxE = fforMaybe respE $ \case
          MTx tx -> Just tx
          _ -> Nothing
    myTxSender u respE
    pure (u <$ closeE, newTxE)

  requestBTCMempool
  void $ btcMempoolTxInserter txE
  where
    switchTuple (a, b) = (switchDyn . fmap leftmost $ a, switchDyn . fmap leftmost $ b)

myTxSender :: (MonadNode t m, MonadStorage t m) => SockAddr -> Event t Message -> m ()
myTxSender addr msgE = do
  pubD <- getPubStorageD
  let txsD = do
        ps <- pubD
        let store = ps ^. btcPubStorage
        let txids = store ^. currencyPubStorage'outgoing
        let txmap = store ^. currencyPubStorage'transactions
        pure (txids, txmap)
  -- Send txs at random moment of times with random subsample of our txs.
  -- That should increase privacy of wallet a bit.
  tickE <- randomTimer 60 300 -- random time between ticks between 1 and 5 minutes.
  txsE <- fmap (fmapMaybe id) $ performEvent $ ffor tickE $ const $ do -- sample random 1/4 txs
    (txids, _) <- sample . current $ txsD
    is <- randomPeekPart 0.25 $ S.toList txids
    pure $ if null is then Nothing else Just is
  performEvent_ $ ffor txsE $ \txs -> logWrite $ "Broadcasting txs: " <> showt txs
  void $ sendRandomNode $ ffor txsE $ \is -> -- send them to random node
    NodeReqBtc . MInv . Inv $ fmap (InvVector InvTx . HT.getTxHash . fromMaybe (error "myTxSender: non BTC outgoing tx") . ETT.toBtcTxHash) is
  -- Answer about known transactions to nodes (also about own outgoing txs that we announce)
  requestManyFromNode $ flip push msgE $ \case
    MGetData (GetData invs) -> Just . (addr,) . uncurry (mkTxMessages invs) <$> sampleDyn txsD
    _ -> pure Nothing
  pure ()

-- | Tick in random time between minT and maxT
randomTimer :: (TriggerEvent t m, PerformEvent t m, PostBuild t m, MonadUnliftIO (Performable m), PlatformNatives) => NominalDiffTime -> NominalDiffTime -> m (Event t ())
randomTimer minT maxT = do
  (tickE, fire) <- newTriggerEvent
  buildE <- getPostBuild
  let tickerE = leftmost [buildE, tickE]
  performFork_ $ ffor tickerE $ const $ liftIO $ do
    t <- randomRIO (toMs minT, toMs maxT)
    threadDelay t
    fire ()
  return tickE
  where
    toMs n = ceiling $ (realToFrac n :: Double) * 1000000

-- | Peek a fraction from 0 to 1 from list
randomPeekPart :: MonadIO m => Float -> [a] -> m [a]
randomPeekPart _ [] = pure []
randomPeekPart k as0 = do
  let n = ceiling $ k * fromIntegral (length as0) :: Int
  go n 0 [] as0
  where
    go _ _ acc [] = pure acc
    go n i acc as
      | i >= n = pure acc
      | otherwise = do
        j <- liftIO $ randomRIO (0, length as - 1)
        let a = as !! j
        let as' = take j as ++ drop (j+1) as
        go n (i+1) (a : acc) as'

-- | Filter TxHashes from Inv vector. Return Nothing if no TxHashes are present
mkTxMessages :: [InvVector] -> S.Set TxId -> M.Map TxId EgvTx -> [NodeReqG]
mkTxMessages invs txids txmap = foo invs [] $ \acc iv -> case invType iv of
  InvTx -> let
    txid    = ETT.BtcTxHash $ TxHash $ invHash iv
    b       = S.member txid txids
    metx    = if b then M.lookup txid txmap else Nothing
    mbtctx  = join $ ffor metx $ \case
      TxBtc (BtcTx tx _) -> Just $ NodeReqBtc $ MTx tx
      _ -> Nothing
    in maybe acc (: acc) mbtctx
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
        txh = ETT.BtcTxHash $ TxHash $ invHash iv
        b = S.member txh txids
        in if b then Nothing else Just iv
      _ -> Nothing

data SAStorageAct = SAAdd [SockAddr] | SARemove SockAddr | SAClear

handleSAStore :: SAStorageAct -> S.Set SockAddr -> Maybe (S.Set SockAddr)
handleSAStore sact acc = case sact of
  SAClear -> Just S.empty
  SAAdd sas -> let
    l = S.size acc
    ltotal = l + length sas
    in if l >= saStorageSize
      then Nothing
      else Just $ S.union acc $ S.fromList $ if ltotal <= saStorageSize
        then sas
        else take (ltotal - saStorageSize) sas
  SARemove sa -> Just $ S.delete sa acc

-- We only want to connect to nodes that support these services:
-- NODE_NETWORK: this node can be asked for full blocks instead of just headers and mempool.
-- NODE_WITNESS: see BIP 0144
hasServices :: NetworkAddress -> Bool
hasServices addr = hasNetwork && hasWitness
  where
    hasNetwork = BI.testBit (naServices addr) 0
    hasWitness = BI.testBit (naServices addr) 3


-- | Creates a dynamic storage for BTC nodes urls
-- Collects saStorageSize urls
-- Takes an event to remove an address from the storage
-- If storage is empty, requests another batch of urls of size saStorageSize
-- Returns the storage and an event which fires first minNodeNum times
-- That event allows the controller to connect to nodes immediately once there is at least 1 connection
mkUrlBatcher :: (MonadNode t m, MonadStatus t m, MonadHasMain m, MonadSettings t m)
  => NodeReqSelector t -> Event t SockAddr -> m (Dynamic t [SockAddr], Event t ())
mkUrlBatcher sel remE = mdo
  buildE <- getPostBuild
  remCntD <- count remE
  let goE = leftmost [Just saStorageSize <$ buildE, actE]
  hostAddrsE <- fmap switchDyn $ networkHold (pure never) $ ffor goE $ \case
    Nothing -> pure never
    Just n -> do
      btcLog $ "Getting a new batch: " <> showt n
      initNodesE <- getRandomBTCNodesFromDNS sel firstTierNodeNum
      fmap switchDyn $ networkHold (pure never) $ ffor initNodesE $ \initNodes -> do
        es <- for initNodes $ \node -> do
          reqE <- fmap (NodeReqBtc MGetAddr <$) getPostBuild
          _ <- requestNodeWait node reqE
          pure $ fforMaybe (nodeconRespE node) $ \case
            MAddr (Addr nats) -> let
              addrs = map snd nats
              verifiedAddrs = filter hasServices addrs
              in Just $ fmap naAddress verifiedAddrs
            _ -> Nothing
        pure $ leftmost es
  sasE <- performFork $ ffor hostAddrsE $ \hs ->
    liftIO $ fmap (SAAdd . catMaybes) $ for hs $ \h ->
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
  let nonNullE = ffilter (not . null) (updated urlsD)
  cntD <- count nonNullE
  fstRunE <- eventToNextFrame $ () <$ ffilter (<= minNodeNum) (updated cntD)
  pure (S.toList <$> urlsD, fstRunE)

-- | Connects to DNS servers, gets n urls and initializes connection to those nodes
getRandomBTCNodesFromDNS :: (MonadNode t m, MonadStatus t m, MonadHasMain m, MonadSettings t m)
  => NodeReqSelector t -> Int -> m (Event t [NodeBtc t])
getRandomBTCNodesFromDNS sel n = do
  buildE <- getPostBuild
  let dnsUrls = getSeeds btcNetwork
  i <- liftIO $ randomRIO (0, length dnsUrls - 1)
  void $ updateWalletStatusNormal BTC $ const WalletStatusNormal'gettingNodeAddresses <$ buildE
  rs <- mkResolvSeed
  urlsE <- performFork $ requestNodesFromBTCDNS rs (dnsUrls!!i) n <$ buildE
  void $ updateWalletStatusNormal BTC $ const (WalletStatusNormal'connectingToPeers BTC) <$ urlsE
  nodesD <- networkHold (pure []) $ ffor urlsE $ \urls -> for urls $ \u -> let
    reqE = extractReq sel BTC u
    in initBtcNode False u reqE
  let connectedE = fforMaybe (updated nodesD) $ \case
        [] -> Nothing
        ns -> Just ns
  void $ updateWalletStatusNormal BTC $ const WalletStatusNormal'synced <$ connectedE
  pure connectedE

-- | Connects to DNS servers and collects n BTC node addresses
requestNodesFromBTCDNS :: (MonadIO m, PlatformNatives) => ResolvSeed -> String -> Int -> m [SockAddr]
requestNodesFromBTCDNS rs dnsurl n = liftIO $ do
  res <- fmap (fromRight []) $ withResolver rs $ \resolver -> lookupA resolver $ B8.pack dnsurl
  urls <- randomVals n res
  pure $ ffor urls $ \u -> let
    h = toHostAddress u
    p = fromIntegral $ getDefaultPort btcNetwork
    in SockAddrInet p h

-- | Pick n random values from a list
randomVals :: MonadIO m => Int -> [a] -> m [a]
randomVals l urls = if l >= n
  then pure urls
  else fmap (urls !!) <$> mkIndexes []
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
