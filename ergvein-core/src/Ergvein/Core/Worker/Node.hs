module Ergvein.Core.Worker.Node
  (
    btcNodeController
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Random
import Data.Either (fromRight)
import Data.IP
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Text (Text)
import Data.Time
import Data.Traversable (for)
import Ergvein.Core.Node
import Ergvein.Core.Platform
import Ergvein.Core.Resolve
import Ergvein.Core.Status
import Ergvein.Core.Store
import Ergvein.Core.Wallet
import Ergvein.Node.Resolve
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public.Btc
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
import Reflex.Workflow
import Sepulcas.Native
import System.IO.Unsafe

import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8
import qualified Data.Dependent.Map as DM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Network.Haskoin.Transaction as HT

-- | How many node connections to establish and keep
targetNodeNum :: Int
targetNodeNum = 5

-- | How many tier-1 nodes to use to populate url cache
firstTierNodeNum :: Int
firstTierNodeNum = 10

-- | How many urls to cache
urlCacheSize :: Int
urlCacheSize = 50

-- | Time to perform handshake, seconds
handshakeTimeout :: NominalDiffTime
handshakeTimeout = 7

btcLog :: (PlatformNatives, MonadIO m) => Text -> m ()
btcLog v = logWrite $ "[nodeController][" <> showt BTC <> "]: " <> v

-- | Check if the counter decreased
handleDecDetector :: Int -> (Bool, Int) -> (Bool, Int)
handleDecDetector n (_, prev) = if n < prev then (True, n) else (False, n)

-- | Track custom node field.
-- If there is a valid custom node, use a simplified private node controller
btcNodeController :: (MonadNode t m, MonadStorage t m, MonadSettings t m
  , MonadWallet t m, MonadStatus t m, MonadHasMain m) => m ()
btcNodeController = do
  customNodeD <- getCustomNodeD
  void $ networkHoldDyn $ ffor customNodeD $ \case
    Nothing -> btcPublicNodeController
    Just url -> do
      let port = if isTestnet then 18333 else 8333
      resolver <- mkResolvSeed
      namedUrl <- resolveAddr resolver port url
      case namedUrl of
        Nothing -> btcPublicNodeController
        Just (NamedSockAddr _ sa) -> btcPrivateNodeController sa

btcPrivateNodeController :: (MonadNode t m, MonadStorage t m, MonadSettings t m
  , MonadWallet t m, MonadStatus t m, MonadHasMain m) => SockAddr -> m ()
btcPrivateNodeController sa = do
  -- This is done to drop all cons in case we switched from pub to private
  clearedE <- clearNodeConns =<< getPostBuild
  btcLog $ "Starting private: <" <> showt sa <> ">"
  sel         <- getNodeNodeReqSelector
  conMapD     <- getNodeConnectionsD
  nodeRef     <- getNodeConnRef
  pubStorageD <- getPubStorageD
  let txidsD  = ffor pubStorageD $ \ps -> M.keysSet $ ps ^. btcPubStorage . currencyPubStorage'transactions
  let reqE = extractReq sel BTC sa
  node <- initBtcNode True 100 sa reqE True
  performEvent $ ffor clearedE $ const $
    modifyExternalRef nodeRef $ \cm -> (addNodeConn (NodeConnBtc node) cm, ())
  let respE = nodeconRespE node
  let txInvsE = flip push respE $ \case
        MInv inv -> do
          txids <- sampleDyn txidsD
          pure $ filterTxInvs txids inv
        _ -> pure Nothing
      reqTxE = (sa,) . NodeReqBtc . MGetData . GetData <$> txInvsE
  _ <- requestFromNode reqTxE
  let newTxE = fforMaybe respE $ \case
        MTx tx -> Just tx
        _ -> Nothing
  myTxSender sa respE
  void $ btcMempoolTxInserter newTxE


btcPublicNodeController :: (MonadNode t m, MonadStorage t m, MonadSettings t m
  , MonadWallet t m, MonadStatus t m, MonadHasMain m) => m ()
btcPublicNodeController = mdo
  btcLog "Starting"
  sel         <- getNodeNodeReqSelector
  conMapD     <- getNodeConnectionsD
  nodeRef     <- getNodeConnRef
  pubStorageD <- getPubStorageD
  initSocks   <- resolveInitialUrls
  let txidsD  = ffor pubStorageD $ \ps -> M.keysSet $ ps ^. btcPubStorage . currencyPubStorage'transactions
  let btcLenD = ffor conMapD $ maybe 0 M.size . DM.lookup BtcTag

  let reqUrlsE = fforMaybe (updated decDetectorD) $ \(b,n) -> if b
        then if n < targetNodeNum then Just (targetNodeNum - n) else Nothing
        else Nothing

  urlsToAddE  <- urlCacheManager initSocks reqUrlsE

  let (remNodeUrlE, txE) = switchTuple $ splitDynPure $ fmap (unzip . M.elems) tmpD
  let addNodeE = (M.fromList . fmap (, Just ())) <$> urlsToAddE
  let remNodeE = (`M.singleton` Nothing) <$> remNodeUrlE
  let listActionE = leftmost [addNodeE, remNodeE]

  -- | Detect when the number of connected nodes decreased
  decDetectorD <- foldDyn handleDecDetector (False, 0) $ updated $ M.size <$> tmpD

  tmpD <- listWithKeyShallowDiff M.empty listActionE $ \u _ _ -> do
    let reqE = extractReq sel BTC u
    let initRating = if u `elem` initSocks then 100 else 50
    node <- initBtcNode True initRating u reqE False
    modifyExternalRef nodeRef $ \cm -> (addNodeConn (NodeConnBtc node) cm, ())
    killE <- delay 0.1 $ nodeconCloseE node
    timeoutE <- delay handshakeTimeout =<< getPostBuild
    let timedoutE = gate (fmap not $ current (nodeconIsUp node)) timeoutE
    closeE <- performEvent $ ffor (leftmost [killE, timedoutE]) $ const $
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

  void $ btcMempoolTxInserter txE
  where
    switchTuple (a, b) = (switchDyn . fmap leftmost $ a, switchDyn . fmap leftmost $ b)

resolveInitialUrls ::(MonadNode t m, MonadStorage t m, MonadSettings t m
  , MonadWallet t m, MonadStatus t m, MonadHasMain m) => m [SockAddr]
resolveInitialUrls = do
  ps <- getPubStorage
  let PubStorageBtc bs = ps ^. btcPubStorage . currencyPubStorage'meta
  let initUrls = S.toList $ _btcPubStorage'preferredNodes bs
  let port = if isTestnet then 18333 else 8333
  resolver <- mkResolvSeed
  namedUrls <- resolveAddrs resolver port initUrls
  pure $ namedAddrSock <$> namedUrls

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
    txs = flip mapMaybe invs $ \case
      iv@(InvVector InvTx hash)
        | S.notMember (ETT.BtcTxHash $ TxHash hash) txids -> Just iv
      _ -> Nothing

-- We only want to connect to nodes that support these services:
-- NODE_NETWORK: this node can be asked for full blocks instead of just headers and mempool.
-- NODE_WITNESS: see BIP 0144
hasServices :: NetworkAddress -> Bool
hasServices addr = hasNetwork && hasWitness
  where
    hasNetwork = BI.testBit (naServices addr) 0
    hasWitness = BI.testBit (naServices addr) 3

-- | Manages the url cache.
-- First targetNodeNum urls are fired as they appear and are not cached
-- Afterwards, collects urlCacheSize urls and provides batches when requested
-- Init socks are appended at the top of the list so they are picked first
urlCacheManager :: forall t m . (MonadNode t m, MonadStatus t m, MonadHasMain m, MonadSettings t m)
    => [SockAddr]               -- ^ Initial urls
    -> Event t Int              -- ^ Event to request a batch of N urls
    -> m (Event t [SockAddr])   -- ^ Url cache
urlCacheManager initSocks reqE = mdo
  (restartE, restartFire) <- newTriggerEvent
  (respE, respFire) <- newTriggerEvent

  -- Fire at most targetNodeNum instantly if we have those
  let (instaUrls, restUrls) = splitAt targetNodeNum initSocks
  buildE <- getPostBuild
  performEvent_ $ ffor buildE $ const $ liftIO $ respFire instaUrls

  -- Put the rest in cache
  urlsRef <- newExternalRef restUrls
  cntVar <- liftIO $ newTVarIO (length instaUrls)
  -- Start batch getter workflow
  batchE <- fmap switchDyn $ workflow (getFirstNodes restartE urlCacheSize)

  performEvent_ $ ffor batchE $ \urls -> liftIO $ do
    n <- readTVarIO cntVar
    let (xs,ys) = splitAt (targetNodeNum - n) urls
    when (not $ null xs) $ do
      respFire xs
      atomically $ writeTVar cntVar (n + length xs)
    modifyExternalRefMaybe_ urlsRef $ addSocksToCache ys
  performEvent_ $ ffor reqE $ \n -> liftIO $ do
    (isEmpty, urls) <- modifyExternalRef urlsRef $ \urls -> let
      (xs,ys) = splitAt n urls
      in (ys, (null ys, xs))
    when isEmpty (restartFire urlCacheSize)
    respFire urls
  pure respE
  where
    -- | Get first firstTierNodeNum node urls from a random dns server
    -- n - max number of urls to collect
    getFirstNodes :: Event t Int -> Int -> Workflow t m (Event t [SockAddr])
    getFirstNodes restartE n = Workflow $ do
      buildE <- getPostBuild
      dnsUrl <- liftIO . uniform $ getSeeds btcNetwork
      rs <- mkResolvSeed
      urlsE <- performFork $ requestNodesFromBTCDNS rs dnsUrl firstTierNodeNum <$ buildE
      pure (never, getSecondNodes restartE n <$> urlsE)

    -- | Connect to first nodes and request nodes from them
    -- Return event with addrs from responses
    getSecondNodes :: Event t Int -> Int -> [SockAddr] -> Workflow t m (Event t [SockAddr])
    getSecondNodes restartE n urls = Workflow $ do
      urlsE <- flip mapM urls $ \u -> mdo
        node <- initBtcNode False 100 u reqE False
        reqE <- eventToNextFrame $ NodeMsgReq (NodeReqBtc MGetAddr) <$ (nodeconOpensE node)
        pure $ fforMaybe (nodeconRespE node) $ \case
          MAddr (Addr nats) -> let
            addrs = map snd nats
            verifiedAddrs = filter hasServices addrs
            in Just $ fmap naAddress verifiedAddrs
          _ -> Nothing
      let resE = fforMaybe (leftmost urlsE) $ \urls -> case catMaybes (tryHostToSockAddr <$> urls) of
            [] -> Nothing
            xs -> Just xs
      -- Count the number of unique urls
      urlCountD <- foldDyn (+) 0 (length <$> resE)
      -- When there is enough urls, switch to finalize to close all widgets
      let enoughE = ffilter (>=n) $ updated urlCountD
      pure (resE, finalize restartE <$ enoughE)

    -- | Wait for restart request, which carries the number of urls to collect
    finalize :: Event t Int -> Workflow t m (Event t [SockAddr])
    finalize restartE = Workflow $ pure (never, getFirstNodes restartE <$> restartE)

    -- | Append socks, capping the cache at urlCacheSize
    addSocksToCache :: [SockAddr] -> [SockAddr] -> Maybe [SockAddr]
    addSocksToCache sas acc = let
      l = length acc
      acc' = L.nub $ acc ++ sas
      ltotal = length acc'
      in if l >= urlCacheSize
        then Nothing
        else Just $ if ltotal <= urlCacheSize
          then acc'
          else take urlCacheSize acc'

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

tryHostToSockAddr :: Network.Haskoin.Network.HostAddress -> Maybe SockAddr
tryHostToSockAddr h = unsafePerformIO $ do
  (Just <$> evaluate (hostToSockAddr h)) `catch` (\ErrorCall{} -> pure Nothing)
