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
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Indexer.Socket

import qualified Data.Map.Strict as M

connectionTimeout :: NominalDiffTime
connectionTimeout = 60

reconnectTimeout :: NominalDiffTime
reconnectTimeout = 5

indexerNodeController :: (MonadIndexClient t m, MonadHasSettings t m) => [NamedSockAddr] -> m ()
indexerNodeController initAddrs = mdo
  nodeLog "Starting"
  sel <- getIndexReqSelector
  (addrE, _) <- getActivationEF
  connRef <- getActiveConnsRef
  let initMap = M.fromList $ ((, ())) <$> initAddrs
      closedE = switchDyn $ ffor valD $ leftmost . M.elems
      delE = (\u -> M.singleton u Nothing) <$> closedE
      addE = (\us -> M.fromList $ (, Just ()) <$> us) <$> addrE
      actE = leftmost [delE, addE]

  performEvent $ ffor delE $ \mnsa -> flip traverse (M.keys mnsa) $
    \(NamedSockAddr _ u) -> nodeLog $ "<" <> showt u <> ">: DELETE"

  valD <- listWithKeyShallowDiff initMap actE $ \nsa@(NamedSockAddr _ u) _ _ -> do
    nodeLog $ "<" <> showt u <> ">: Connect"
    te <- tickLossyFromPostBuildTime 10
    performEvent $ ffor te $ const $ nodeLog $ "<" <> showt u <> ">: Ping"
    let reqE = select sel $ Const2 u
    conn <- initIndexerConnection nsa reqE
    modifyExternalRef connRef $ \cm -> (M.insert u conn cm, ())

    -- Everything below thsi line is handling the closure of a connection
    -- the event the socket fires when it wants to be closed
    let closedE' = indexConClosedE conn
    performEvent_ $ ffor closedE' $ const $ nodeLog $ "<" <> showt u <> ">: closedE'"
    -- potential solution: directly send signal to kill the widget
    -- from this point when IndexerClose message is sent
    let clsMsgE = never
    -- let clsMsgE = fforMaybe reqE $ \case
    --       IndexerClose -> Just ()
    --       _ -> Nothing
    failedToConnectE <- connectionWidget conn

    -- closedE'' -- init closure procedure here
    let closedE'' = leftmost [closedE', failedToConnectE, clsMsgE]
    -- remove the connection from the connection map
    closedE''' <- performEvent $ ffor closedE'' $ const $ modifyExternalRef connRef $ \cm -> (M.delete u cm, ())
    -- send out the event to delete this widget
    pure $ nsa <$ closedE'''
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
