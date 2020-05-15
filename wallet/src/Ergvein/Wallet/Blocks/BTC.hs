module Ergvein.Wallet.Blocks.BTC
  (
    requestBTCBlockNode
  , requestBTCBlockConfirm
  , requestBTCBlockRN
  , storeBlockByE
  , storeMultipleBlocksByE
  , getBlockByE
  , storeBlockTxHashesByE
  , storeMultipleBlocksTxHashesByE
  , requestBTCBlocksWait
  , requestBTCBlocksWaitRN
  , requestBTCBlocksListen
  , requestBTCBlocksListenRN
  , BlockRequestMessage(..)
  , module Ergvein.Wallet.Blocks.BTC.Queries
  ) where

import Control.Monad.Random
import Data.Maybe (catMaybes)
import Network.Haskoin.Network
import Network.Haskoin.Block

import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Node
import Ergvein.Wallet.Util
import Ergvein.Wallet.Blocks.BTC.Queries
import Ergvein.Wallet.Blocks.Storage

import qualified Data.Dependent.Map as DM
import qualified Data.Map as M
import qualified Data.List as L

import Ergvein.Wallet.Native

-- | Request a block from a node
--   Returns Nothing if the blockhash is not found in the inventory
requestBTCBlockNode :: MonadFront t m => NodeBTC t -> Event t BlockHash -> m (Event t (Maybe Block))
requestBTCBlockNode node@NodeConnection{..} bhE = do
  -- Send a request once the connection is established
  requestNodeWait node $ ffor bhE $ \bh ->
    NodeReqBTC $ MGetData $ GetData [InvVector InvBlock $ getBlockHash bh]
  fmap switchDyn $ widgetHold (pure never) $ ffor bhE $ \bh -> do
    let respE = fforMaybe nodeconRespE $ \case
          MBlock blk -> if headerHash (blockHeader blk) == bh   -- Check if it's the block we asked for
            then Just (Just blk)
            else Nothing
          -- Check that the block hash is in "not found" message
          MNotFound (NotFound invs) -> let
            blockInv = InvVector InvBlock $ getBlockHash bh
            in case filter (blockInv ==) invs of
              [] -> Nothing
              _ -> Just Nothing
          _ -> Nothing
    headE respE         -- Return only the first such event. Just in case

data BlockRequestMessage = BRMBlock !Block | BRMNotFound | BRMConflict

-- | Request a block from n nodes and check that they all return the same block
requestBTCBlockConfirm :: MonadFront t m => Int -> Event t BlockHash -> m (Event t BlockRequestMessage)
requestBTCBlockConfirm n reqE = do
  conMapD <- getNodeConnectionsD
  fmap switchDyn $ widgetHoldDyn $ ffor conMapD $ \cm -> case DM.lookup BTCTag cm of
    Nothing -> pure never
    Just btcs -> do
      let nodes = take n $ M.elems btcs
          n'    = length nodes
      if n' == 0 then pure never else do
        -- Run n' requests (n' might be lower than n)
        respE <- fmap leftmost $ flip traverse nodes $ \node -> do
          respE <- requestBTCBlockNode node reqE
          pure $ (nodeconUrl node,) <$> respE
        -- Accumulate responses in a map
        responsesD <- foldDyn (\(u,mv) m -> M.insert u mv m) M.empty respE
        pure $ fforMaybe (updated responsesD) $ \respMap -> if M.size respMap /= n'
          then Nothing                                      -- Not all responses are collected yet
          else case L.nub $ catMaybes (M.elems respMap) of  -- Filter "Not found"s
            [] -> Just BRMNotFound
            blk:[] -> Just $ BRMBlock blk
            _  -> Just BRMConflict

-- | Request a block from a random connected node
--   Returns Nothing if the blockhash is not found in the inventory
requestBTCBlockRN :: MonadFront t m => Event t BlockHash -> m (Event t (Maybe Block))
requestBTCBlockRN reqE = do
  conMapD <- getNodeConnectionsD
  fmap switchDyn $ widgetHoldDyn $ ffor conMapD $ \cm -> case DM.lookup BTCTag cm of
    Nothing -> pure never
    Just btcsMap -> case M.elems btcsMap of
      [] -> pure never
      btcs -> do
        let n = length btcs
        i <- liftIO $ randomRIO (0, n - 1)
        requestBTCBlockNode (btcs!!i) reqE

-- | Request multiple blocks from a node
--   Wait until there is a response for each block from the request
requestBTCBlocksWait :: MonadFront t m => NodeBTC t -> Event t [BlockHash] -> m (Event t [Block])
requestBTCBlocksWait node reqE = do
  let respE = nodeconRespE node
  -- Send a request once the connection is established
  requestNodeWait node $ ffor reqE $ \bhs ->
    NodeReqBTC $ MGetData $ GetData $ fmap (InvVector InvBlock . getBlockHash) bhs
  fmap switchDyn $ widgetHold (pure never) $ ffor reqE $ \bhs -> do
    let updE = fforMaybe respE $ \case
          MBlock blk -> let
            bh = headerHash $ blockHeader blk
            in if bh `L.elem` bhs
                  then Just $ [(bh, Just blk)]
                  else Nothing
          MNotFound (NotFound invs) -> case catMaybes $ (filt bhs) <$> invs of
            [] -> Nothing
            vals -> Just vals
          _ -> Nothing

    responsesD <- foldDyn (\vals m0 -> L.foldl' (\m (u,mv) -> M.insert u mv m) m0 vals) M.empty updE
    pure $ fforMaybe (updated responsesD) $ \respMap -> if M.size respMap /= length bhs
      then Nothing
      else Just $ catMaybes $ M.elems respMap
  where
    filt :: [BlockHash] -> InvVector -> Maybe (BlockHash, Maybe Block)
    filt bhs (InvVector ivt ivh) = case ivt of
      InvBlock -> let bh = BlockHash ivh
        in if bh `L.elem` bhs then Just (bh, Nothing) else Nothing

-- | Request multiple blocks from a random connected node
--   Wait until there is a response for each block from the request
requestBTCBlocksWaitRN :: MonadFront t m => Event t [BlockHash] -> m (Event t [Block])
requestBTCBlocksWaitRN reqE = do
  logWrite "[requestBTCBlocksWaitRN]: Request"
  conMapD <- getNodeConnectionsD
  fmap switchDyn $ widgetHoldDyn $ ffor conMapD $ \cm -> case DM.lookup BTCTag cm of
    Nothing -> pure never
    Just btcsMap -> case M.elems btcsMap of
      [] -> pure never
      btcs -> do
        let n = length btcs
        i <- liftIO $ randomRIO (0, n - 1)
        requestBTCBlocksWait (btcs!!i) reqE

-- | Request multiple blocks from a node
--   Return an event, which fires whenever one of the blocks form the request is received
requestBTCBlocksListen :: MonadFront t m => NodeBTC t -> Event t [BlockHash] -> m (Event t Block)
requestBTCBlocksListen node reqE = do
  let respE = nodeconRespE node
  -- Send a request once the connection is established
  requestNodeWait node $ ffor reqE $ \bhs ->
    NodeReqBTC $ MGetData $ GetData $ fmap (InvVector InvBlock . getBlockHash) bhs
  fmap switchDyn $ widgetHold (pure never) $ ffor reqE $ \bhs -> do
    pure $ fforMaybe respE $ \case
      MBlock blk -> let bh = headerHash $ blockHeader blk
        in if bh `L.elem` bhs then Just blk else Nothing
      _ -> Nothing

-- | Request multiple blocks from a random connected node
--   Return an event, which fires whenever one of the blocks form the request is received
requestBTCBlocksListenRN :: MonadFront t m => Event t [BlockHash] -> m (Event t Block)
requestBTCBlocksListenRN reqE = do
  conMapD <- getNodeConnectionsD
  fmap switchDyn $ widgetHoldDyn $ ffor conMapD $ \cm -> case DM.lookup BTCTag cm of
    Nothing -> pure never
    Just btcsMap -> case M.elems btcsMap of
      [] -> pure never
      btcs -> do
        let n = length btcs
        i <- liftIO $ randomRIO (0, n - 1)
        requestBTCBlocksListen (btcs!!i) reqE

storeBlockByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t Block -> m (Event t ())
storeBlockByE = performEvent . fmap insertBtcBlock

storeMultipleBlocksByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t [Block] -> m (Event t ())
storeMultipleBlocksByE = performEvent . fmap insertMultipleBtcBlocks

getBlockByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t BlockHash -> m (Event t (Maybe Block))
getBlockByE = performEvent . fmap getBtcBlock

storeBlockTxHashesByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t Block -> m (Event t ())
storeBlockTxHashesByE = performEvent . fmap insertBtcBlockTxHashesToBlockHash

storeMultipleBlocksTxHashesByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t [Block] -> m (Event t ())
storeMultipleBlocksTxHashesByE = performEvent . fmap insertMultipleBtcBlocksTxHashesToBlockHash
