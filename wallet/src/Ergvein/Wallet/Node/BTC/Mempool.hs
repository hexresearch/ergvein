module Ergvein.Wallet.Node.BTC.Mempool
  (
    requestBTCMempool
  ) where

import Control.Monad.Random
import Network.Haskoin.Network

import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Native
import Ergvein.Wallet.Util
import Ergvein.Text
import Ergvein.Wallet.Node.Types

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M

-- | Request a mempool from a random node
requestBTCMempool :: MonadFront t m => m ()
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
mempoolRequester :: MonadFront t m => SockAddr -> NodeBTC t -> m (Event t ())
mempoolRequester addr NodeConnection{..} = do
  buildE      <- getPostBuild
  let upE     = leftmost [updated nodeconIsUp, current nodeconIsUp `tag` buildE]
  let btcreq  = NodeReqBTC MMempool
  let reqE    = fforMaybe upE $ \b -> if b then Just (nodeconUrl, btcreq) else Nothing
  performEvent_ $ ffor reqE $ const $ logWrite $ "Requesting initial mempool for BTC from " <> showt addr
  _ <- requestFromNode reqE
  let hugeInv = fforMaybe nodeconRespE $ \case
        MInv (Inv is) -> if length is >= mempoolMinInvSize then Just () else Nothing
        _ -> Nothing
  pure hugeInv
