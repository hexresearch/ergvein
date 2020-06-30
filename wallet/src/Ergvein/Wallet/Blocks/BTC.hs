module Ergvein.Wallet.Blocks.BTC
  (
    storeBlockByE
  , storeMultipleBlocksByE
  , getBlockByE
  , storeBlockTxHashesByE
  , storeMultipleBlocksTxHashesByE
  , requestBTCBlocks
  , module Ergvein.Wallet.Blocks.BTC.Queries
  ) where

import Control.Monad.Random
import Data.Maybe (catMaybes)
import Network.Haskoin.Network
import Network.Haskoin.Block

import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Node
import Ergvein.Wallet.Blocks.BTC.Queries
import Ergvein.Wallet.Blocks.Storage
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.List as L

data RBTCBlksAct = RASucc [Block] | RANoNode [BlockHash] | RANodeClosed [BlockHash]


-- | Request a number of blocks from a random node.
-- waits for a reply about all blocks before firing the resulting event
-- Retries after a second if there are no active nodes
-- Retries after 0.05s if the node disconnected (picks another one)
-- Once the result is returned, close the blocksRequester widget and stop paying attention to the node
requestBTCBlocks :: MonadFront t m => Event t [BlockHash] -> m (Event t [Block])
requestBTCBlocks reqE = mdo
  conMapD <- getNodeConnectionsD
  let goE =  attach (current conMapD) $ leftmost [reqE, noNodeE, nodeCloseE]
  let goE' = leftmost [Just <$> goE, Nothing <$ resE]
  actE <- fmap switchDyn $ widgetHold (pure never) $ ffor goE' $ \case
    Nothing -> pure never
    Just (cm, req) -> do
      buildE <- eventToNextFrame =<< getPostBuild
      case DM.lookup BTCTag cm of
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

storeBlockByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t Block -> m (Event t ())
storeBlockByE = performEvent . fmap insertBtcBlock

storeMultipleBlocksByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t [Block] -> m (Event t [Block])
storeMultipleBlocksByE e = performEvent $ ffor e $ \blocks -> insertMultipleBtcBlocks blocks >> pure blocks

getBlockByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t BlockHash -> m (Event t (Maybe Block))
getBlockByE = performEvent . fmap getBtcBlock

storeBlockTxHashesByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t Block -> m (Event t ())
storeBlockTxHashesByE = performEvent . fmap insertBtcBlockTxHashesToBlockHash

storeMultipleBlocksTxHashesByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t [Block] -> m (Event t [Block])
storeMultipleBlocksTxHashesByE e = performEvent $ ffor e $ \blocks -> insertMultipleBtcBlocksTxHashesToBlockHash blocks >> pure blocks
