module Ergvein.Wallet.Worker.Node
  (
    btcNodeRefresher
  ) where

import Control.Monad.Random
import Data.Maybe (fromMaybe, catMaybes)
import Network.DNS
import Network.Haskoin.Constants
import Network.Haskoin.Network
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
import qualified Data.Text as T
import qualified Data.Bits as BI
import qualified Data.ByteString.Char8 as B8

minNodeNum :: Int
minNodeNum = 3

btcLog :: (PlatformNatives, MonadIO m) => Text -> m ()
btcLog v = logWrite $ "[nodeRefresher][" <> showt BTC <> "]: " <> v

btcNodeRefresher :: MonadFront t m => m ()
btcNodeRefresher = do
  btcLog "Starting"
  buildE  <- getPostBuild
  sel     <- getNodeRequestSelector
  nodeRef <- getNodeConnRef
  conMapE <- updatedWithInit =<< getNodeConnectionsD
  let extraE = fforMaybe conMapE $ \cm -> let
        l = fromMaybe 0 $ fmap M.size $ DM.lookup BTCTag cm
        in if l >= minNodeNum then Nothing else Just (minNodeNum - l)

  goE <- fmap updated $ foldDyn (\n (a,_) -> (a+1, n)) (0, 0) $ extraE
  let goMapE = uncurry M.singleton . fmap Just <$> goE
  void $ listWithKeyShallowDiff mempty goMapE $ \_ n _ -> do
    btcLog $ "Getting extra nodes: " <> showt n
    let dnsUrls = getSeeds btcNetwork
    i <- liftIO $ randomRIO (0, length dnsUrls - 1)
    urls <- requestNodesFromBTCDNS (dnsUrls!!i) n
    nodes <- flip traverse urls $ \u -> let
      reqE = extractReq sel BTC u
      in fmap NodeConnBTC $ initBTCNode u reqE
    btcLog $ showt . fmap showBaseUrl $ urls
    modifyExternalRef nodeRef $ \cm -> (addMultipleConns cm nodes, ())

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
