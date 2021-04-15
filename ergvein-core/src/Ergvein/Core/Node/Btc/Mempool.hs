module Ergvein.Core.Node.Btc.Mempool
  (
    requestBTCMempool
  ) where

import Control.Monad.Random
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Core.Node.Btc
import Ergvein.Text
import Network.Haskoin.Network
import Network.Socket (SockAddr)
import Reflex
import Reflex.Workflow
import Reflex.Flunky
import Sepulcas.Native

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M

-- | Request a mempool from a random node
requestBTCMempool :: MonadNode t m => m ()
requestBTCMempool = void $ workflow waitNode
  where
    waitNode = Workflow $ do
      conMapE <- updatedWithInit =<< getBtcNodesD
      nodeE <- performEvent $ ffor conMapE $ \cm ->
        if M.null cm then pure Nothing else do
          let btcs = M.toList cm
          node <- liftIO $ fmap (btcs!!) $ randomRIO (0, length btcs - 1)
          pure $ Just node
      pure ((), uncurry loadMempool <$> fmapMaybe id nodeE)

    loadMempool addr node = Workflow $ do
      timeoutE <- delay 10 =<< getPostBuild -- Node can drop us before we asked for mempool
      requestedE <- mempoolRequester addr node
      let nextE = leftmost [doNothing <$ requestedE, waitNode <$ timeoutE]
      pure ((), nextE)

    doNothing = Workflow $ do
      logWrite "Mempool requester is disabled. We got enough of mempool today..."
      -- buildE <- delay 3 =<< getPostBuild
      let nextE = never -- TODO: request mempool here if we implement background sleeping
      pure ((), waitNode <$ nextE)

-- | Consider this size as minimum size of mempool that we expect as answer for mempool msg
mempoolMinInvSize :: Int
mempoolMinInvSize = 20

-- | Requester for mempool
mempoolRequester :: MonadNode t m => SockAddr -> NodeBtc t -> m (Event t ())
mempoolRequester addr NodeConnection{..} = do
  buildE      <- getPostBuild
  let upE     = leftmost [updated nodeconIsUp, current nodeconIsUp `tag` buildE]
  let btcreq  = NodeReqBtc MMempool
  let reqE    = fforMaybe upE $ \b -> if b then Just (nodeconUrl, btcreq) else Nothing
  performEvent_ $ ffor reqE $ const $ logWrite $ "Requesting initial mempool for BTC from " <> showt addr
  _ <- requestFromNode reqE
  let hugeInv = fforMaybe nodeconRespE $ \case
        MInv (Inv is) -> if length is >= mempoolMinInvSize then Just () else Nothing
        _ -> Nothing
  pure hugeInv
