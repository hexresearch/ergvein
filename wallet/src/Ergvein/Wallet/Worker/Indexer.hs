module Ergvein.Wallet.Worker.Indexer
  (
    indexerNodeController
  ) where

import Data.Functor.Misc (Const2(..))
import Network.Socket (SockAddr)
import Reflex.Dom
import Reflex.ExternalRef

import Ergvein.Text
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Native
import Ergvein.Wallet.Indexer.Socket

import qualified Data.Map.Strict as M

indexerNodeController :: MonadIndexClient t m => [SockAddr] -> m ()
indexerNodeController initAddrs = mdo
  nodeLog "Starting"
  sel <- getIndexReqSelector
  (addrE, _) <- getActivationEF
  connRef <- getActiveConnsRef
  let initMap = M.fromList $ ((, ())) <$> initAddrs
  initE <- fmap (initMap <$) getPostBuild
  let closedE = switchDyn $ ffor valD $ leftmost . M.elems
  let delE = (\u -> M.singleton u Nothing) <$> closedE
  let addE = (\u -> M.singleton u (Just ())) <$> addrE
  let actE = leftmost [delE, addE]
  valD <- listWithKeyShallowDiff initMap actE $ \u _ _ -> do
    nodeLog $ "<" <> showt u <> ">: Connect"
    let reqE = select sel $ Const2 u
    conn <- initIndexerConnection u reqE
    modifyExternalRef connRef $ \cm -> (M.insert u conn cm, ())
    closedE' <- delay 0.1 $ indexConClosedE conn
    closedE'' <- performEvent $ ffor closedE' $ const $ modifyExternalRef connRef $ \cm -> (M.delete u cm, ())
    connectionWidget conn
    pure $ u <$ closedE''
  pure ()
  where
    nodeLog t = logWrite $ "[indexerNodeController]: " <> t

connectionWidget :: MonadIndexClient t m => IndexerConnection t -> m ()
connectionWidget IndexerConnection{..} = do
  performEvent $ (nodeLog . showt) <$> indexConRespE
  performEvent $ (nodeLog "OPENED") <$ indexConOpensE

  pure ()
  where
    nodeLog t = logWrite $ "[connectionWidget]<" <> showt indexConAddr <> ">: " <> t
