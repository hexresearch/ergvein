module Ergvein.Core.Client.Mempool
  (
  ) where

import Control.Monad.Random
import Ergvein.Core.Node.Btc ()
import Ergvein.Core.Node.Monad
import Ergvein.Core.Node.Types
import Ergvein.Core.Store.Monad
import Ergvein.Text
import Ergvein.Types.Storage
import Network.Haskoin.Network
import Network.Socket (SockAddr)
import Reflex
import Reflex.Workflow
import Reflex.Flunky
import Sepulcas.Native
import Ergvein.Core.Wallet.Monad
import Ergvein.Index.Protocol.Types hiding (CurrencyCode(..))
import Ergvein.Types

import qualified Data.Map.Strict as M

-- | Request a mempool from a random node
requestBTCMempool :: (MonadWallet t m, MonadNode t m) => m ()
requestBTCMempool = void $ workflow waitRestore
  where
    waitRestore = Workflow $ do
      nextE <- updatedWithInit =<< (fmap . fmap) _pubStorage'restoring getPubStorageD
      pure ((), requestMempool <$ nextE)
    requestMempool = Workflow $ do
      buildE <- getPostBuild
      respE <- requestRandomIndexer $ (BTC, MGetMemFilters GetMemFilters) <$ buildE
      let memfiltersE = fforMaybe respE $ \(_, msg) -> case msg of
            MMemFilters (FilterTree ft) -> Just ft
            _ -> Nothing
      pure ((), never)

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
