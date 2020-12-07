module Ergvein.Wallet.Worker.Height
  (
    heightAsking
  ) where

import Control.Lens
import Data.Time
import Data.Maybe
import Network.Haskoin.Block
import Network.Haskoin.Network

import Ergvein.Wallet.Platform
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Types.Storage.Currency.Public.Btc
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Native
import Ergvein.Wallet.Node
import Ergvein.Wallet.Status.Types
import Ergvein.Wallet.Util

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

emptyHash :: BlockHash
emptyHash = "0000000000000000000000000000000000000000000000000000000000000000"

-- | Retry polling height each 5 seconds on errors
heightReqTimeout :: NominalDiffTime
heightReqTimeout = 5

heightAsking :: (MonadFront t m) => m ()
heightAsking = void . widgetHoldDyn . fmap (traverse_ heightAsker) =<< getActiveCursD

heightAsker :: (MonadFront t m) => Currency -> m ()
heightAsker cur = case cur of
  BTC -> heightAskerBtc
  _ -> pure ()

-- | Height asker is implemented via three workflows:
--                                 +------------+
--                                 v            |
--     +----------------+       +--+------------+--+       +-----------------+
--     |  startBTCFlow  +------>+  btcCatchUpFlow  +------>+  btcListenFlow  |
--     +------+---------+       +------------------+       +--------+--------+
--            ^                                                     |
--            +-----------------------------------------------------+
-- description for each stage is provided at each respective flow
heightAskerBtc :: MonadFront t m => m ()
heightAskerBtc = do
  logWrite $ "[heightAsking][BTC]: Start worker"
  void $ workflow startBTCFlow

-- | BTC workflow starter. Picks the last block locator and starts the btcCatchUpFlow
startBTCFlow :: MonadFront t m => Workflow t m ()
startBTCFlow = Workflow $ do
  buildE <- getPostBuild
  ps <- getPubStorage
  let bl0 = fromMaybe (error "btcNodeController: cannot get block headers!") $ ps ^? pubStorage'currencyPubStorages
        . at BTC . _Just . currencyPubStorage'meta
        . _PubStorageBtc . btcPubStorage'headerSeq
      bl = fmap V.toList $ if V.null $ snd bl0 then btcCheckpoints else bl0
  pure ((), btcCatchUpFlow bl <$ buildE)

-- | Requests block headers exhaustively until it catches up with the chain head
-- once it catches up -- it switches the flow to btcListenFlow
btcCatchUpFlow :: MonadFront t m => (Timestamp, [(BlockHeight, BlockHash)]) -> Workflow t m ()
btcCatchUpFlow (ts, bl) = Workflow $ do
  let (h0, lasthash) = head bl
  logWrite $ "btcCatchUpFlow: " <> showt h0
  buildE <- getPostBuild
  storedE <-  attachNewBtcHeader "btcCatchUpFlow" False $ (h0, ts, lasthash) <$ buildE
  publishStatusUpdate $ CurrencyStatus BTC (StatGettingHeight $ fromIntegral h0) <$ storedE
  let req = MGetHeaders $ GetHeaders 70012 (snd <$> bl) emptyHash
  respE <- requestRandomNode $ (NodeReqBTC req) <$ storedE
  let hlE = fforMaybe respE $ \case
        NodeRespBTC (MHeaders (Headers hl)) -> Just $ fmap fst hl
        _ -> Nothing
  let succE = ffor hlE $ \case
        [] -> btcListenFlow lasthash ts h0
        hs -> let
          h' = h0 + (fromIntegral $ length hs)
          hd = last hs
          hhash = headerHash hd
          in if hhash == lasthash
            then btcListenFlow lasthash ts h'
            else btcCatchUpFlow (blockTimestamp hd, (h', hhash):bl)
  timeoutE <- delay heightReqTimeout buildE
  nextE <- delay 1 $ leftmost [btcCatchUpFlow (ts, bl) <$ timeoutE, succE]
  pure ((), nextE)


-- | Listen to Inv messages. Once a new inv with InvBlock blockhash is received
-- request the block's header and check:
-- if the header refers to the last seen block
--   then update the last seen block and increment the height
--   else if blockTimestamp is higher than the last seen
--     then something is wrong, assume that we missed a block and try to catch up via btcCatchUpFlow
--     else assume that we have already seen the block and ignore the header
btcListenFlow :: MonadFront t m => BlockHash -> Timestamp -> BlockHeight -> Workflow t m ()
btcListenFlow h0 ts0 he0 = Workflow $ mdo
  logWrite $ "[heightAsking][BTC]: Start btcListenFlow at height " <> showt he0
  buildE <- getPostBuild
  htD <- holdDyn (he0, ts0, h0) setE
  connsD <- getNodeConnectionsD
  let respE = switchDyn $ ffor connsD $
        leftmost . fmap nodeconRespE . M.elems . fromMaybe M.empty . DM.lookup BTCTag
      bhE = fforMaybe respE $ \case
        MInv (Inv invs) -> pickFirstBlockInv invs
        _ -> Nothing
      reqE = poke bhE $ \bh -> do
        (_, _, ha) <- sampleDyn htD
        pure $ NodeMsgReq $ NodeReqBTC $ MGetHeaders $ GetHeaders 70012 [ha] bh
  broadcastNodeMessage BTC reqE
  let hlE = fforMaybe respE $ \case
        MHeaders (Headers hl) -> case hl of
          [] -> Nothing
          _ -> Just $ fst $ last hl
        _ -> Nothing
      actE = flip push hlE $ \hd -> do
        (he, ts, ha) <- sampleDyn htD
        pure $ if prevBlock hd == ha
          then Just $ Right (he + 1, blockTimestamp hd, headerHash hd)
          else if blockTimestamp hd > ts
            then Just $ Left ()
            else Nothing
      restartE = fmapMaybe (either Just (const Nothing)) actE
      setE = fmapMaybe (either (const Nothing) Just) actE
      storeE = leftmost [(he0, ts0, h0) <$ buildE, updated htD]
  void $ attachNewBtcHeader "btcListenFlow" True storeE
  publishStatusUpdate $ ffor storeE $ \(he,_,_) -> CurrencyStatus BTC Synced
  pure ((), startBTCFlow <$ restartE)

pickFirstBlockInv :: [InvVector] -> Maybe BlockHash
pickFirstBlockInv invs = case invs of
  [] -> Nothing
  x:xs -> case invType x of
    InvBlock -> Just $ BlockHash $ invHash x
    _ -> pickFirstBlockInv xs
