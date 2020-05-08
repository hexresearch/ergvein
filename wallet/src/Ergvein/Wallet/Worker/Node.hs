module Ergvein.Wallet.Worker.Node
  (
    btcNodeRefresher
  ) where

import Control.Monad.Random
import Data.IP
import Data.Maybe (fromMaybe)
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

import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8

minNodeNum :: Int
minNodeNum = 3

firstTierNodeNum :: Int
firstTierNodeNum = 6

btcLog :: (PlatformNatives, MonadIO m) => Text -> m ()
btcLog v = logWrite $ "[nodeRefresher][" <> showt BTC <> "]: " <> v

btcRefrTimeout :: NominalDiffTime
btcRefrTimeout = 30

btcNodeRefresher :: MonadFront t m => m ()
btcNodeRefresher = do
  btcLog "Starting"
  sel     <- getNodeRequestSelector
  nodeRef <- getNodeConnRef
  conMapD <- getNodeConnectionsD
  let btcLenD = ffor conMapD $ fromMaybe 0 . fmap M.size . DM.lookup BTCTag
  buildE <- getPostBuild
  te <- fmap void $ tickLossyFromPostBuildTime btcRefrTimeout
  let reqExtraE = attachWithMaybe (\l _ -> if l >= minNodeNum then Nothing else Just (minNodeNum - l))
                    (current btcLenD) $ leftmost [te, buildE]
  rec
    let extraE = leftmost [Just <$> reqExtraE, Nothing <$ nodesE]
    extraUrlsE <- fmap switchDyn $ widgetHold (pure never) $ ffor extraE $ \case
      Nothing -> pure never
      Just n -> do
        btcLog $ "Getting extra nodes: " <> showt n
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

    urlsD <- foldDynMaybe handleSAStore [] $ leftmost [SAAdd <$> extraUrlsE, SAClear <$ reqExtraE]
    let goE = fforMaybe (updated urlsD) $ \um -> if length um >= minNodeNum then Just um else Nothing

    cntD <- foldDyn (\urls (n,_) -> (n + 1, Just urls)) ((0 :: Int), Nothing) goE
    let nodesE = updated $ (uncurry M.singleton) <$> cntD
  void $ listWithKeyShallowDiff mempty nodesE $ \_ urls _ -> do
    nodes <- flip traverse urls $ \u -> do
      let reqE = extractReq sel BTC u
      fmap NodeConnBTC $ initBTCNode u reqE
    modifyExternalRef nodeRef $ \cm -> (addMultipleConns cm nodes, ())


data SAStorageAct = SAAdd [SockAddr] | SAClear

handleSAStore :: SAStorageAct -> [SockAddr] -> Maybe [SockAddr]
handleSAStore sact um = case sact of
  SAClear -> Just []
  SAAdd sas -> let
    l = length um
    l' = length sas
    ltotal = l + l'
    n = if ltotal <= minNodeNum then l' else ltotal - minNodeNum
    sas' = take n sas
    in if l >= minNodeNum
      then Nothing
      else Just $ take minNodeNum $ L.nub $ um <> sas'

getRandomBTCNodesFromDNS :: MonadFrontConstr t m => RequestSelector t -> Int -> m (Event t [NodeBTC t])
getRandomBTCNodesFromDNS sel n = do
  buildE <- getPostBuild
  let dnsUrls = getSeeds btcNetwork
  i <- liftIO $ randomRIO (0, length dnsUrls - 1)
  urlsE <- performFork $ (requestNodesFromBTCDNS (dnsUrls!!i) n) <$ buildE
  nodesD <- widgetHold (pure []) $ ffor urlsE $ \urls -> flip traverse urls $ \u -> let
    reqE = extractReq sel BTC u
    in initBTCNode u reqE
  pure $ fforMaybe (updated nodesD) $ \case
    [] -> Nothing
    ns -> Just ns

requestNodesFromBTCDNS :: (MonadIO m, PlatformNatives) => String -> Int -> m [SockAddr]
requestNodesFromBTCDNS dnsurl n = liftIO $ do
  rs <- makeResolvSeed nativeResolvConf
  res <- fmap (either (const []) id) $ withResolver rs $ \resolver -> lookupA resolver $ B8.pack dnsurl
  urls <- randomVals n res
  pure $ ffor urls $ \u -> let
    h = toHostAddress u
    p = fromIntegral $ getDefaultPort btcNetwork
    in SockAddrInet p h

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
