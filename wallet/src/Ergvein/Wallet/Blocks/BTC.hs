module Ergvein.Wallet.Blocks.BTC
  (
    requestBTCBlockNode
  , requestBTCBlockConfirm
  , requestBTCBlockRandNode
  , storeBlockByE
  , getBlockByE
  , BlockRequestMessage(..)
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

-- | Request a block from a node
requestBTCBlockNode :: MonadFront t m => NodeBTC t -> Event t BlockHash -> m (Event t (Maybe Block))
requestBTCBlockNode node@NodeConnection{..} bhE = do
  -- Send a request when the connection is established
  requestNodeWait node $ ffor bhE $ \bh ->
    MGetData $ GetData [InvVector InvBlock $ getBlockHash bh]
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

-- Request a block from n nodes and check that they return the same block
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

-- Request a block from a random connected node
requestBTCBlockRandNode :: MonadFront t m => Event t BlockHash -> m (Event t (Maybe Block))
requestBTCBlockRandNode reqE = do
  conMapD <- getNodeConnectionsD
  fmap switchDyn $ widgetHoldDyn $ ffor conMapD $ \cm -> case DM.lookup BTCTag cm of
    Nothing -> pure never
    Just btcsMap -> case M.elems btcsMap of
      [] -> pure never
      btcs -> do
        let n = length btcs
        i <- liftIO $ randomRIO (0, n - 1)
        requestBTCBlockNode (btcs!!i) reqE

storeBlockByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t Block -> m (Event t ())
storeBlockByE = performEvent . fmap insertBTCBlock

getBlockByE :: (MonadBaseConstr t m, HasBlocksStorage (Performable m)) => Event t BlockHash -> m (Event t (Maybe Block))
getBlockByE = performEvent . fmap getBTCBlock
