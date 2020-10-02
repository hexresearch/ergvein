module Ergvein.Wallet.Worker.Indexer
  (
    indexerNodeController
  ) where

import Control.Monad
import Data.Functor.Misc (Const2(..))
import Data.Time
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
  reconnectTimeoutE <- delay reconnectTimeout closedE
  let initMap = M.fromList $ ((, ())) <$> initAddrs
      closedE = switchDyn $ ffor valD $ leftmost . M.elems
      delE = (\u -> M.singleton u Nothing) <$> closedE
      addE = (\us -> M.fromList $ (, Just ()) <$> us) <$> addrE
      actE = leftmost [delE, addE, reconnectE]
      reconnectE = (\u -> M.singleton u (Just ())) <$> reconnectTimeoutE
  valD <- listWithKeyShallowDiff initMap actE $ \u _ _ -> do
    nodeLog $ "<" <> showt u <> ">: Connect"
    let reqE = select sel $ Const2 u
    conn <- initIndexerConnection u reqE
    modifyExternalRef connRef $ \cm -> (M.insert u conn cm, ())
    closedE' <- delay 0.1 $ indexConClosedE conn
    failedToConnectE <- connectionWidget conn
    let closedE'' = leftmost [closedE', failedToConnectE]
    closedE''' <- performEvent $ ffor closedE'' $ const $ modifyExternalRef connRef $ \cm -> (M.delete u cm, ())
    pure $ u <$ closedE'''
  pure ()
  where
    nodeLog t = logWrite $ "[indexerNodeController]: " <> t

connectionWidget :: MonadIndexClient t m => IndexerConnection t -> m (Event t ())
connectionWidget IndexerConnection{..} = do
  performEvent_ $ (nodeLog "Connected") <$ indexConOpensE
  timeoutE <- void <$> tickLossyFromPostBuildTime connectionTimeout
  pure $ gate (not <$> current indexConIsUp) timeoutE
  where
    nodeLog t = logWrite $ "[indexerNodeController]<" <> showt indexConAddr <> ">: " <> t

connectionTimeout :: NominalDiffTime
connectionTimeout = 5

reconnectTimeout :: NominalDiffTime
reconnectTimeout = 5