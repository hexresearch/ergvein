module Ergvein.Wallet.Worker.Node
  (
    bctNodeController
  ) where

import Control.Exception
import Control.Monad.Random
import Data.IP
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Time
import Network.DNS
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Socket
import Reflex.ExternalRef

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Monad.Async
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node
import Ergvein.Wallet.Node.BTC
import Ergvein.Wallet.Platform
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8

minNodeNum :: Int
minNodeNum = 3

firstTierNodeNum :: Int
firstTierNodeNum = 4

saStorageSize :: Int
saStorageSize = 100

btcLog :: (PlatformNatives, MonadIO m) => Text -> m ()
btcLog v = logWrite $ "[nodeController][" <> showt BTC <> "]: " <> v

btcRefrTimeout :: NominalDiffTime
btcRefrTimeout = 10

bctNodeController :: MonadFront t m => m ()
bctNodeController = mdo
  btcLog "Starting"
  sel       <- getNodeRequestSelector
  conMapD   <- getNodeConnectionsD
  nodeRef   <- getNodeConnRef
  te        <- fmap void $ tickLossyFromPostBuildTime btcRefrTimeout
  let btcLenD = ffor conMapD $ fromMaybe 0 . fmap M.size . DM.lookup BTCTag
  let te' = current btcLenD `tag` te
  -- Get an url to connect if:
  -- 1. BTC conMap is updated
  -- 2. The first minNodeNum times an urls is added to the storage
  -- 3. Suppose 1st event fired and the storage is empty, then try again after btcRefrTimeout
  let tickE = leftmost [updated btcLenD, 0 <$ fstRunE, te']
  let urlE = flip push tickE $ \l -> if l >= minNodeNum
        then pure Nothing
        else fmap listToMaybe $ sampleDyn urlStoreD
  (urlStoreD, fstRunE) <- mkUrlBatcher sel urlE

  let addNodeE = (\u -> M.singleton u $ Just ()) <$> urlE
  let remNodeE = fmap (\u -> M.singleton u Nothing) $ switchDyn $ fmap (leftmost . M.elems) tmpD
  let listActionE = leftmost [addNodeE, remNodeE]

  tmpD <- listWithKeyShallowDiff M.empty listActionE $ \u _ _ -> do
    let reqE = extractReq sel BTC u
    node <- initBTCNode True u reqE
    modifyExternalRef nodeRef $ \cm -> (addNodeConn (NodeConnBTC node) cm, ())
    closeE <- performEvent $ ffor (nodeconCloseE node) $ const $
      modifyExternalRef nodeRef $ \cm -> (removeNodeConn BTCTag u cm, ())
    pure $ u <$ closeE
  -- dbgPrintE $ (showt . length) <$> (updated urlStoreD)
  -- dbgPrintE $ showt <$> listActionE
  pure ()

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
  => RequestSelector t -> Event t SockAddr -> m (Dynamic t [SockAddr], Event t ())
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
  let sactE = leftmost [sasE, SARemove <$> remE]
  urlsD <- foldDynMaybe handleSAStore S.empty $ leftmost [sasE, SARemove <$> remE]
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
getRandomBTCNodesFromDNS :: MonadFrontConstr t m => RequestSelector t -> Int -> m (Event t [NodeBTC t])
getRandomBTCNodesFromDNS sel n = do
  buildE <- getPostBuild
  let dnsUrls = getSeeds btcNetwork
  i <- liftIO $ randomRIO (0, length dnsUrls - 1)
  urlsE <- performFork $ (requestNodesFromBTCDNS (dnsUrls!!i) n) <$ buildE
  nodesD <- widgetHold (pure []) $ ffor urlsE $ \urls -> flip traverse urls $ \u -> let
    reqE = extractReq sel BTC u
    in initBTCNode False u reqE
  pure $ fforMaybe (updated nodesD) $ \case
    [] -> Nothing
    ns -> Just ns

-- | Connects to DNS servers and collects n BTC node addresses
requestNodesFromBTCDNS :: (MonadIO m, PlatformNatives) => String -> Int -> m [SockAddr]
requestNodesFromBTCDNS dnsurl n = liftIO $ do
  rs <- makeResolvSeed nativeResolvConf
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
