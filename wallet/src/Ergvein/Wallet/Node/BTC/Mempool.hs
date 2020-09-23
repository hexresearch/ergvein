module Ergvein.Wallet.Node.BTC.Mempool
  (
    requestBTCMempool
  ) where

import Control.Monad.Random
import Network.Haskoin.Network

import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Node.Types

import qualified Data.Dependent.Map as DM
import qualified Data.Map.Strict as M

data MempoolAnswers = MANoNode | MAEmpty | MAAnswer Message deriving (Show, Eq)

-- | Request a mempool from a random node
requestBTCMempool :: MonadFront t m => Event t () -> m (Event t MempoolAnswers)
requestBTCMempool reqE = mdo
  conMapD <- getNodeConnectionsD
  let goE =  attach (current conMapD) $ reqE
  actE <- fmap switchDyn $ widgetHold (pure never) $ ffor goE $ \(cm, _) -> do
    buildE <- getPostBuild
    case DM.lookup BTCTag cm of
      Nothing -> pure $ MANoNode <$ buildE
      Just btcsMap -> case M.elems btcsMap of
        [] -> pure $ MAEmpty <$ buildE
        btcs -> do
          node <- liftIO $ fmap (btcs!!) $ randomRIO (0, length btcs - 1)
          mempoolRequester node
  pure actE


-- | Requester for mempool
mempoolRequester :: MonadFront t m => NodeBTC t -> m (Event t MempoolAnswers)
mempoolRequester NodeConnection{..} = do
  buildE      <- getPostBuild
  let upE     = leftmost [updated nodeconIsUp, current nodeconIsUp `tag` buildE]
  let btcreq  = NodeReqBTC MMempool
  let reqE    = fforMaybe upE $ \b -> if b then Just (nodeconUrl, btcreq) else Nothing
  let updE    = fmap (\a -> MAAnswer a) nodeconRespE
  requestFromNode reqE
  pure $ updE
