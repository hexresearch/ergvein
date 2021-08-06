module Ergvein.Core.Node.Btc.Headers(
    HeadersTree(..)
  , addHeader
  , getBestHeight
  , newHeadersTree
  ) where

import Data.Map (Map)
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Ergvein.Core.Platform
import Network.Haskoin.Block
import Network.Haskoin.Block.Headers (BlockNode(..), validBlock, headerWork)

import qualified Data.Map.Strict as M

data HeadersTree = HeadersTree {
  headersStartHeight :: !BlockHeight
, headersNodes       :: !(Map BlockHash BlockNode)
, headersBest        :: !(Maybe BlockHash)
}

newHeadersTree :: BlockHeight -> HeadersTree
newHeadersTree h = HeadersTree h mempty Nothing

addHeader :: UTCTime -> BlockHeader -> HeadersTree -> Either String HeadersTree
addHeader t bh tree@HeadersTree{..} = case getBestBlock of
  Nothing -> insertRoot
  Just bestNode -> case getParentBlock of
    Nothing -> insertRoot
    Just parentNode -> do
      let parents = collectParents 10 parentNode tree
      newNode <- validBlock btcNetwork t' bestNode parentNode parents bh parentNode
      pure HeadersTree {
          headersStartHeight = headersStartHeight
        , headersNodes = M.insert (headerHash bh) newNode headersNodes
        , headersBest = theBest
        }
  where
    t' = round . utcTimeToPOSIXSeconds $ t :: Timestamp
    getBestBlock = do
      bestHash <- headersBest
      M.lookup bestHash headersNodes
    getParentBlock = M.lookup (prevBlock bh) headersNodes
    theBest = case headersBest of
        Just curBestHash -> case M.lookup curBestHash headersNodes of
          Just curBest | nodeWork curBest > headerWork bh -> Just curBestHash
          _ -> Just $ headerHash bh
        _ -> Just $ headerHash bh
    insertRoot = do
      let hash = headerHash bh
          node = BlockNode bh (headersStartHeight+1) (headerWork bh) hash
      pure HeadersTree {
          headersStartHeight = headersStartHeight
        , headersNodes = M.insert hash node headersNodes
        , headersBest = theBest
        }

collectParents :: Int -> BlockNode -> HeadersTree -> [BlockNode]
collectParents n0 bn0 tree = reverse $ go [] n0 bn0
  where
    go acc 0 _ = acc
    go acc n bn = case M.lookup (prevBlock $ nodeHeader bn) $ headersNodes tree of
      Nothing -> acc
      Just pbn -> go (pbn : acc) (n-1) pbn

getBestHeight :: HeadersTree -> BlockHeight
getBestHeight HeadersTree{..} = fromMaybe headersStartHeight $ do
  bh <- headersBest
  node <- M.lookup bh headersNodes
  pure $ nodeHeight node
