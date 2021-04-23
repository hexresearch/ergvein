module Ergvein.Core.Node.Btc.Blocks
  ( getStartHeightBTC
  , requestBTCBlocks
  ) where

import Control.Monad.Random
import Data.Maybe (catMaybes)
import Data.Time
import Data.Word
import Network.Haskoin.Block
import Network.Haskoin.Network
import Reflex
import Reflex.Network
import Reflex.Flunky

import Ergvein.Core.Node.Btc
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.List as L

data RBTCBlksAct = RASucc [Block] | RANoNode [BlockHash] | RANodeClosed [BlockHash]

getStartHeightBTC :: MonadNode t m => m (Dynamic t (Maybe Word32))
getStartHeightBTC = do
  conMapD <- getBtcNodesD
  let heightD = join $ ffor conMapD $ \connMap ->
        L.foldl' (\d1 d2 -> ffor2 d1 d2 max) (constDyn Nothing) (nodeconHeight <$> M.elems connMap)
  pure heightD

-- | Amount of seconds we give a node to send us blocks either retry.
blockTimeout :: NominalDiffTime
blockTimeout = 20

-- | Request a number of blocks from a random node.
-- waits for a reply about all blocks before firing the resulting event
-- Retries after a second if there are no active nodes
-- Retries after 0.05s if the node disconnected (picks another one)
-- Once the result is returned, close the blocksRequester widget and stop paying attention to the node
requestBTCBlocks :: MonadNode t m => Event t [BlockHash] -> m (Event t [Block])
requestBTCBlocks reqE = mdo
  conMapD <- getNodeConnectionsD
  timeE <- tickLossyFromPostBuildTime blockTimeout
  blksD <- holdDyn [] reqE
  let timeHE = tag (current blksD) timeE
  let goE =  attach (current conMapD) $ leftmost [reqE, noNodeE, nodeCloseE, timeHE]
  let goE' = leftmost [Just <$> goE, Nothing <$ resE]
  actE <- fmap switchDyn $ networkHold (pure never) $ ffor goE' $ \case
    Nothing -> pure never
    Just (cm, req) -> do
      buildE <- eventToNextFrame =<< getPostBuild
      case DM.lookup BtcTag cm of
        Nothing -> pure $ RANoNode req <$ buildE
        Just btcsMap -> case M.elems btcsMap of
          [] -> pure $ RANoNode req <$ buildE
          btcs -> do
            node <- liftIO $ fmap (btcs!!) $ randomRIO (0, length btcs - 1)
            blocksRequester req node
  noNodeE <- delay 1 $ fforMaybe actE $ \case
    RANoNode bhs -> Just bhs
    _ -> Nothing
  nodeCloseE <- delay 0.05 $ fforMaybe actE $ \case
    RANodeClosed bhs -> Just bhs
    _ -> Nothing
  let resE = fforMaybe actE $ \case
        RASucc blks -> Just blks
        _ -> Nothing
  pure resE

-- | Requester for requestBTCBlocks
blocksRequester :: MonadNode t m => [BlockHash] -> NodeBtc t -> m (Event t RBTCBlksAct)
blocksRequester bhs NodeConnection{..} = do
  buildE      <- getPostBuild
  let upE     = leftmost [updated nodeconIsUp, current nodeconIsUp `tag` buildE]
  let btcreq  = NodeReqBtc $ MGetData $ GetData $ fmap (InvVector InvBlock . getBlockHash) bhs
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
