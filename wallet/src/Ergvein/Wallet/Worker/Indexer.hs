module Ergvein.Wallet.Worker.Indexer
  (
    indexerNodeController
  ) where

import Control.Monad
import Control.Monad.Random
import Data.Functor.Misc (Const2(..))
import Data.Time
import Reflex.Dom
import Reflex.ExternalRef
import Reflex.Flunky
import Sepulcas.Native

import Ergvein.Index.Protocol.Types
import Ergvein.Node.Constants
import Ergvein.Node.Resolve
import Ergvein.Node.Resolve
import Ergvein.Text
import Ergvein.Wallet.Indexer.Socket
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Monad.Util
import Ergvein.Wallet.Settings

import qualified Data.Map.Strict as M

connectionTimeout :: NominalDiffTime
connectionTimeout = 60

latencyCheckInterval :: NominalDiffTime
latencyCheckInterval = 4

indexerNodeController :: (MonadHasUI m, MonadIndexClient t m, MonadHasSettings t m) => [ErgveinNodeAddr] -> m ()
indexerNodeController initAddrs = mdo
  nodeLog "Starting"
  sel <- getIndexReqSelector
  (addrE, _) <- getActivationEF
  connRef <- getActiveConnsRef
  seed <- mkResolvSeed
  let initMap = M.fromList $ ((, ())) <$> initAddrs
      closedE = switchDyn $ ffor valD $ leftmost . M.elems
      delE = (\u -> M.singleton u (Just ())) <$> closedE
      addE = (\us -> M.fromList $ (, Just ()) <$> us) <$> addrE
      actE = leftmost [delE, addE]
  valD <- listWithKeyShallowDiff initMap actE $ \u _ _ -> do
    mAddr <- resolveAddr seed defIndexerPort u
    liftIO $ print  $ ()<$ mAddr
    case mAddr of
      Just addr -> do
        nodeLog $ "<" <> showt u <> ">: Connect"
        let reqE = select sel $ Const2 u 
        conn <- initIndexerConnection u (namedAddrSock addr) reqE
        modifyExternalRef connRef $ \cm -> (M.insert u conn cm, ())
        indexerStatusUpdater conn
        -- Everything below this line is handling the closure of a connection
        -- the event the socket fires when it wants to be closed
        let closedE' = indexConClosedE conn
        failedToConnectE <- connectionWidget conn
        -- closedE'' -- init closure procedure here
        timeoutE <- (\x ->switchDyn $ ffor (indexConIsUp conn) ( \up -> if up then x else never )) <$> connectionLatencyWidget conn
        --timeoutE <- connectionLatencyWidget conn
        let closedE'' = leftmost [closedE', failedToConnectE, timeoutE]
        -- remove the connection from the connection map
        closedE''' <- performEvent $ ffor closedE'' $ const $ modifyExternalRef connRef $ \cm -> (M.delete u cm, ())
        -- reopen on latency check failure
        --reopenAndWait $ (namedAddrName addr) <$ timeoutE
        -- send out the event to delete this widget
        pure $ traceEvent "________________closedE'''" $ u <$ closedE'''
      _ -> (\z-> traceEvent "________________mAddr" $ u <$ z) <$> getPostBuild 
  pure ()
  where
    nodeLog t = logWrite $ "[indexerNodeController]: " <> t

connectionLatencyWidget :: forall t m . MonadIndexClient t m => IndexerConnection t -> m (Event t ())
connectionLatencyWidget connection = mdo
  startE <- getPostBuild
  pongE <- pingNode =<< delay latencyCheckInterval (leftmost [pingE, startE])
  (pingE, timeoutE) <- switchDyn2 <$> workflow (body pongE)
  pure timeoutE
  where
    body :: Event t () -> Workflow t m (Event t (), Event t ())
    body pongE =  Workflow $ do 
      timeoutE <- delay (latencyCheckInterval * 2) =<< getPostBuild
      let nextE = body pongE <$ pongE
      pure ((pongE,  timeoutE), nextE)
    pingNode :: Event t () -> m (Event t ()) 
    pingNode e = do
      fireReq  <- getIndexReqFire
      performFork_ $ ffor e $ const $ liftIO $ do
        pingPayload <- randomIO
        fireReq $ M.singleton (indexConName connection) $ (IndexerMsg $ MPing pingPayload)
      pure $ fforMaybe (indexConRespE connection) $ \case
                      MPong pingPayload ->  Just ()
                      _                 ->  Nothing

connectionWidget :: MonadIndexClient t m => IndexerConnection t -> m (Event t ())
connectionWidget IndexerConnection{..} = do
  performEvent_ $ (nodeLog "Connected") <$ indexConOpensE
  timeoutE <- void <$> tickLossyFromPostBuildTime connectionTimeout
  pure $ gate (not <$> current indexConIsUp) timeoutE
  where
    nodeLog t = logWrite $ "[indexerNodeController]<" <> showt indexConAddr <> ">: " <> t
