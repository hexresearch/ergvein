module Ergvein.Core.Worker.Indexer
  (
    indexerNodeController
  ) where

import Control.Monad
import Data.Functor.Misc (Const2(..))
import Data.Time
import Ergvein.Core.Client
import Ergvein.Core.Resolve
import Ergvein.Core.Settings
import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Ergvein.Text
import Reflex
import Reflex.ExternalRef
import Reflex.Main.Thread
import Sepulcas.Native

import qualified Data.Map.Strict as M

connectionTimeout :: NominalDiffTime
connectionTimeout = 60

indexerNodeController :: (PlatformNatives, MonadHasMain m, MonadIndexClient t m, MonadSettings t m)
  => [ErgveinNodeAddr] -> m ()
indexerNodeController initAddrs = mdo
  nodeLog "Starting"
  sel <- getIndexReqSelector
  (addrE, _) <- getActivationEF
  connRef <- getActiveConnsRef
  seed <- mkResolvSeed
  let initMap = M.fromList $ ((, ())) <$> initAddrs
      closedE = switchDyn $ ffor valD $ leftmost . M.elems
      delE = (\u -> M.singleton u Nothing) <$> closedE
      addE = (\us -> M.fromList $ (, Just ()) <$> us) <$> addrE
      actE = leftmost [delE, addE]
  valD <- listWithKeyShallowDiff initMap actE $ \u _ _ -> do
    mAddr <- resolveAddr seed defIndexerPort u
    case mAddr of
      Just addr -> do
        nodeLog $ "<" <> showt u <> ">: Connect"
        let reqE = select sel $ Const2 u
        conn <- initIndexerConnection u (namedAddrSock addr) reqE
        modifyExternalRef connRef $ \cm -> (M.insert u conn cm, ())
        indexerStatusUpdater conn
        -- Everything below thsi line is handling the closure of a connection
        -- the event the socket fires when it wants to be closed
        let closedE' = indexConClosedE conn
        failedToConnectE <- connectionWidget conn
        -- closedE'' -- init closure procedure here
        let closedE'' = leftmost [closedE', failedToConnectE]
        -- remove the connection from the connection map
        closedE''' <- performEvent $ ffor closedE'' $ const $ modifyExternalRef connRef $ \cm -> (M.delete u cm, ())
        -- send out the event to delete this widget
        pure $ u <$ closedE'''
      _ -> pure never
  pure ()
  where
    nodeLog t = logWrite $ "[indexerNodeController]: " <> t

connectionWidget :: (PlatformNatives, MonadIndexClient t m) => IndexerConnection t -> m (Event t ())
connectionWidget IndexerConnection{..} = do
  performEvent_ $ (nodeLog "Connected") <$ indexConOpensE
  timeoutE <- void <$> tickLossyFromPostBuildTime connectionTimeout
  pure $ gate (not <$> current indexConIsUp) timeoutE
  where
    nodeLog t = logWrite $ "[indexerNodeController]<" <> showt indexConAddr <> ">: " <> t
