module Ergvein.Core.Node.Btc.Blocks
  ( requestBlocksBtc
  ) where

import Control.Monad.Random
import Data.Maybe (catMaybes, isNothing)
import Data.Time
import Network.Haskoin.Block
import Network.Haskoin.Network
import Reflex
import Reflex.ExternalRef
import Reflex.Flunky
import Reflex.Network
import Reflex.Workflow
import Sepulcas.Native
import Ergvein.Text
import Ergvein.Types.Currency
import Network.Socket

import Ergvein.Core.Node.Btc
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Core.Store.Monad

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S


-- | Amount of seconds we give a node to send us blocks either retry.
blockTimeout :: NominalDiffTime
blockTimeout = 20

-- | Timeout in case there is no active connections
noNodeTimeout :: NominalDiffTime
noNodeTimeout = 5

-- | Handles requesting logic
requestBlocksBtc :: forall t m . (MonadNode t m, MonadStorage t m) => Event t [BlockHash] -> m (Event t [Block])
requestBlocksBtc reqE = fmap switchDyn $ workflow $ waitForRequest
  where
    -- | Wait for a request to come. Filter empty requests just in case
    waitForRequest :: Workflow t m (Event t [Block])
    waitForRequest = Workflow $ do
      let nonEmptyReqE = ffilter (not . null) reqE
      pure (never, selectNode <$> nonEmptyReqE)

    -- | Node selection process.
    -- Node is selected randomly from weighted distibution, where the weight is a node's rating
    -- If there is no active connections, wait noNodeTimeout and try again
    selectNode :: [BlockHash] -> Workflow t m (Event t [Block])
    selectNode req = Workflow $ do
      cons <- fmap M.elems . sampleDyn =<< getBtcNodesD
      mcon <- liftIO $ do
        consRated <- flip traverse cons $ \c -> do
          r <- readExternalRef $ nodeconRating c
          pure (c, fromIntegral . unNodeRating $ r)
        fromListMay consRated
      buildE <- getPostBuild
      goE <- case mcon of
        Nothing -> do
          logWrite $ "No active connections"
          delay noNodeTimeout $ selectNode req <$ buildE
        Just con -> (fmap . fmap) (const $ runReq req con) $ eventToNextFrame buildE
      pure (never, goE)

    -- | Run the request to the selected node
    -- Blocks are collected in a Map BlockHash (Maybe Block)
    -- If the node says that a block was not found, Nothing is added to the map
    -- Once the map is filled, respond to the caller with blocks
    -- Run blocks which were not found through a different node
    -- Reward the node afterwards
    -- If runReq times out, punish the node and run the request again
    runReq :: [BlockHash] -> NodeBtc t -> Workflow t m (Event t [Block])
    runReq bhs con@NodeConnection{..} = Workflow $ do
      let btcreq  = NodeReqBtc $ MGetData $ GetData $ fmap (InvVector InvBlock . getBlockHash) bhs
      buildE <- getPostBuild
      timeoutE <- void <$> tickLossyFromPostBuildTime blockTimeout
      _ <- requestFromNode $ (nodeconUrl, btcreq) <$ buildE
      let updE = fforMaybe nodeconRespE $ \case
            MBlock blk -> let
              bh = headerHash $ blockHeader blk
              in if bh `L.elem` bhs
                    then Just $ [(bh, Just blk)]
                    else Nothing
            MNotFound (NotFound invs) -> case catMaybes $ filt bhs <$> invs of
              [] -> Nothing
              vals -> Just vals
            _ -> Nothing
      responsesD <- foldDyn (\vals m0 -> L.foldl' (\m (u,mv) -> M.insert u mv m) m0 vals) M.empty updE
      let resE = fforMaybe (updated responsesD) $ \respMap -> if M.size respMap /= length bhs
            then Nothing
            else let
              blks = catMaybes $ M.elems respMap
              rereqs = S.toList $ M.keysSet $ M.filter isNothing respMap
              in Just (blks, rereqs)
      let punishE = punish bhs con <$ leftmost [timeoutE, nodeconCloseE]
      rewardE <- delay 0.1 $ (rewardNode con . snd) <$> resE
      goE <- eventToNextFrame $ leftmost [ rewardE, punishE ]
      pure (fst <$> resE, goE)

    -- | Punish a node which has timed out or disconnected
    punish :: [BlockHash] -> NodeBtc t -> Workflow t m (Event t [Block])
    punish req con = Workflow $ do
      logWrite "Timed out"
      goE <- handleRating (-10) con
      pure (never, selectNode req <$ goE)

    -- | Reward a node which succesfully responded to a request
    -- If the node has found all blocks (rereq is []) then reward with 10, else 5
    rewardNode :: NodeBtc t -> [BlockHash] -> Workflow t m (Event t [Block])
    rewardNode con rereqs = Workflow $ do
      let reward = if null rereqs then 10 else 5
      doneE <- handleRating reward con
      let nextE = ffor doneE $ const $ if null rereqs
            then waitForRequest else selectNode rereqs
      pure (never, never)

    -- | Handle node rating
    -- Close the connection, remove or add the node to the list of preferred nodes as needed
    handleRating :: Int -> NodeBtc t -> m (Event t ())
    handleRating delta con = do
      let sa = nodeconUrl con
      buildE <- getPostBuild
      storedE <- performEvent $ ffor buildE $ const $
        modifyExternalRef (nodeconRating con) $ \r -> let
          d = fromIntegral $ abs delta
          r' = if (signum delta >= 0) then r + d else r - d
          in (r',(r, r'))
      let (killE, (remE, addE)) = fmap fanEither $ fanEither $ fmapMaybe (rate sa) storedE

      fmap leftmost $ sequence
        [ postNodeMessage BTC killE
        , addSuperbBtcNode addE
        , removeSuperbBtcNode remE
        ]

    -- | Handle rating and return apropriate action. Which constructor is which is obvious from the usage in handleRating
    rate :: SockAddr -> (NodeRating, NodeRating) -> Maybe (Either (SockAddr, NodeMessage) (Either SockAddr SockAddr))
    rate sa (rnew,rold) = case checkRating rnew of
      RLRemove -> Just $ Left (sa, NodeMsgClose)
      RLAcceptable -> if isSuperbRating rold then Just (Right $ Left sa) else Nothing
      RLSuperb -> if isSuperbRating rold then Nothing else Just (Right $ Right sa)

    -- | Filter blocks that we have requested
    filt :: [BlockHash] -> InvVector -> Maybe (BlockHash, Maybe Block)
    filt bhs (InvVector ivt ivh) = case ivt of
      InvBlock -> let bh = BlockHash ivh
        in if bh `L.elem` bhs then Just (bh, Nothing) else Nothing
      _ -> Nothing
