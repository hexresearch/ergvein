module Ergvein.Wallet.Worker.Node
  (
    btcNodeRefresher
  ) where

import Control.Monad.Random
import Data.Maybe (fromMaybe, catMaybes)
import Network.DNS
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Socket
import Reflex.ExternalRef
import Servant.Client(BaseUrl(..), showBaseUrl, parseBaseUrl)
import Control.Concurrent
import Control.Concurrent.Lifted (fork)

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
import qualified Data.Text as T
import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8

minNodeNum :: Int
minNodeNum = 3

firstTierNodeNum :: Int
firstTierNodeNum = 6

btcLog :: (PlatformNatives, MonadIO m) => Text -> m ()
btcLog v = logWrite $ "[nodeRefresher][" <> showt BTC <> "]: " <> v

btcNodeRefresher :: MonadFront t m => m ()
btcNodeRefresher = do
  btcLog "Starting"
  buildE  <- getPostBuild
  sel     <- getNodeRequestSelector
  nodeRef <- getNodeConnRef
  conMapE <- updatedWithInit =<< getNodeConnectionsD
  let reqExtraE = fforMaybe conMapE $ \cm -> let
        l = fromMaybe 0 $ fmap M.size $ DM.lookup BTCTag cm
        in if l >= minNodeNum then Nothing else Just (minNodeNum - l)

  rec
    let extraE = leftmost [Just <$> reqExtraE, Nothing <$ initNodesE]
    extraUrlsE <- fmap switchDyn $ widgetHold (pure never) $ ffor extraE $ \case
      Nothing -> pure never
      Just n -> do
        btcLog $ "Getting extra nodes: " <> showt n
        initNodesE <- getRandomBTCNodesFromDNS sel firstTierNodeNum
        fmap switchDyn $ widgetHold (pure never) $ ffor initNodesE $ \initNodes -> do
          es <- flip traverse initNodes $ \node -> do
            buildE <- getPostBuild
            let reqE = (NodeReqBTC MGetAddr) <$ buildE
            requestNodeWait node reqE
            pure $ fforMaybe (nodeconRespE node) $ \case
                    MAddr (Addr urls) -> Just $ fmap (naAddress . snd) urls
                    _ -> Nothing
          pure $ leftmost es

    urlsD <- foldDynMaybe handleSAStore S.empty $ leftmost [SAAdd <$> extraUrlsE, SAClear <$ reqExtraE]

    let goE = fforMaybe (updated urlsD) $ \um -> if S.size um >= minNodeNum then Just um else Nothing

    cntD <- foldDyn (\urls (n,_) -> (n + 1, Just urls)) (0, Nothing) goE
    let initNodesE = updated $ (uncurry M.singleton) <$> cntD

  void $ listWithKeyShallowDiff mempty initNodesE $ \_ urls _ -> do
    nodes <- flip traverse (S.toList urls) $ \u -> do
      let reqE = extractReq sel BTC u
      fmap NodeConnBTC $ initBTCNode u reqE
    modifyExternalRef nodeRef $ \cm -> (addMultipleConns cm nodes, ())


data SAStorageAct = SAAdd [SockAddr] | SAClear

handleSAStore :: SAStorageAct -> S.Set BaseUrl -> Maybe (S.Set BaseUrl)
handleSAStore sact um = case sact of
  SAClear -> Just S.empty
  SAAdd sas -> if S.size um >= minNodeNum then Nothing else let
    bus = catMaybes . fmap (parseBaseUrl . show) . filter isIPv4 $ sas
    bum = S.fromList bus
    in Just $ S.union um bum
  where
    isIPv4 :: SockAddr -> Bool
    isIPv4 sa = case sa of
      SockAddrInet{} -> True
      _ -> False

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

requestNodesFromBTCDNS :: MonadIO m => String -> Int -> m [BaseUrl]
requestNodesFromBTCDNS dnsurl n = liftIO $ do
  rs <- makeResolvSeed defaultResolvConf
  res <- fmap (either (const []) id) $ withResolver rs $ \resolver -> lookupA resolver $ B8.pack dnsurl
  urls <- randomVals n res
  pure $ catMaybes $ ffor urls $ \u -> let
    h = show u
    p = show $ getDefaultPort btcNetwork
    in parseBaseUrl $ h <> ":" <> p

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
