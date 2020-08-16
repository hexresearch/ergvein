  module Ergvein.Wallet.Node.BTC.Mempool
  (
    requestBTCMempool
  ) where

import Control.Monad.Random
import Data.Maybe (catMaybes)
import Data.Time
import Network.Haskoin.Block
import Network.Haskoin.Network

import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Node.Types
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.List as L

data RBTCBlksAct = RASucc [Block] | RANoNode [BlockHash] | RANodeClosed [BlockHash]

-- | Amount of seconds we give a node to send us blocks either retry.
blockTimeout :: NominalDiffTime
blockTimeout = 20

-- | Request a number of blocks from a random node.
-- waits for a reply about all blocks before firing the resulting event
-- Retries after a second if there are no active nodes
-- Retries after 0.05s if the node disconnected (picks another one)
-- Once the result is returned, close the blocksRequester widget and stop paying attention to the node
requestBTCMempool :: MonadFront t m => Event t () -> m (Event t (Maybe Message))
requestBTCMempool reqE = mdo
  conMapD <- getNodeConnectionsD
  let goE =  attach (current conMapD) $ reqE
  actE <- fmap switchDyn $ widgetHold (pure never) $ ffor goE $ \(cm, req) -> do
    buildE <- getPostBuild
    case DM.lookup BTCTag cm of
      Nothing -> pure $ Nothing <$ buildE
      Just btcsMap -> case M.elems btcsMap of
        [] -> pure $ Nothing <$ buildE
        btcs -> do
          node <- liftIO $ fmap (btcs!!) $ randomRIO (0, length btcs - 1)
          mempoolRequester node
  pure actE

-- | Requester for requestBTCBlocks
blocksRequester :: MonadFront t m => [BlockHash] -> NodeBTC t -> m (Event t RBTCBlksAct)
blocksRequester bhs NodeConnection{..} = do
  buildE      <- getPostBuild
  let upE     = leftmost [updated nodeconIsUp, current nodeconIsUp `tag` buildE]
  let btcreq  = NodeReqBTC $ MGetData $ GetData $ fmap (InvVector InvBlock . getBlockHash) bhs
  let reqE    = fforMaybe upE $ \b -> if b then Just (nodeconUrl, btcreq) else Nothing
  let updE    = fforMaybe nodeconRespE $ \case
        MBlock blk -> let
          bh = headerHash $ blockHeader blk
          in if bh `L.elem` bhs
                then Just $ [(bh, Just blk)]
                else Nothing
        MNotFound (NotFound invs) -> case catMaybes $ filt <$> invs of
          [] -> Nothing
          vals -> Just vals
        _ -> Nothing
  requestFromNode reqE
  responsesD <- foldDyn (\vals m0 -> L.foldl' (\m (u,mv) -> M.insert u mv m) m0 vals) M.empty updE
  let resE = fforMaybe (updated responsesD) $ \respMap -> if M.size respMap /= length bhs
        then Nothing
        else Just $ RASucc $ catMaybes $ M.elems respMap
  pure $ leftmost [resE, RANodeClosed bhs <$ nodeconCloseE]
  where
    filt :: InvVector -> Maybe (BlockHash, Maybe Block)
    filt (InvVector ivt ivh) = case ivt of
      InvBlock -> let bh = BlockHash ivh
        in if bh `L.elem` bhs then Just (bh, Nothing) else Nothing
      _ -> Nothing



-- | Requester for requestBTCBlocks
mempoolRequester :: MonadFront t m => NodeBTC t -> m (Event t (Maybe Message))
mempoolRequester NodeConnection{..} = do
  buildE      <- getPostBuild
  let upE     = leftmost [updated nodeconIsUp, current nodeconIsUp `tag` buildE]
  let btcreq  = NodeReqBTC $ MMempool
  let reqE    = fforMaybe upE $ \b -> if b then Just (nodeconUrl, btcreq) else Nothing
  let updE    = fmap (\a -> Just a) nodeconRespE
  requestFromNode reqE
  pure $ updE
