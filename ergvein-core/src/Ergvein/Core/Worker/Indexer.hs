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
import Reflex.Workflow
import Control.Concurrent
import qualified Data.Map.Strict as M
import Network.DNS

import Ergvein.Index.Protocol.Types
import Reflex.Flunky
import Control.Monad
import Control.Monad.Random
import Reflex.Fork
import Ergvein.Core.Node.Socket

connectionTimeout :: NominalDiffTime
connectionTimeout = 60
latencyCheckInterval = 4

indexerNodeController :: (PlatformNatives, MonadHasMain m, MonadClient t m, MonadSettings t m)
  => [ErgveinNodeAddr] -> m ()
indexerNodeController initialAddresses = mdo
  let initialAddressMap = M.fromList $ (, ()) <$> initialAddresses
      addrConnClosedE = switchDyn $ leftmost . M.elems <$> activeNodesD
      addrAddE = M.fromList . fmap (, Just ()) <$> addrE
      addrDelE = (`M.singleton` Nothing) . fst <$> addrConnClosedE
      reconnectNeededE = fforMaybe addrConnClosedE $ \case
       (addrToReconnect, True) -> Just addrToReconnect 
       _-> Nothing
  
  addressReconnectionCountMapRef <- newExternalRef M.empty
  addrReconnectE <- performFork $ ffor reconnectNeededE $ \addrToReconnect -> do
      reconnectTryIndex <- modifyExternalRef addressReconnectionCountMapRef $ \oldMap -> let
        newMap = M.alter (Just . maybe 0 succ) addrToReconnect oldMap
        in (newMap, newMap M.! addrToReconnect)
      liftIO $ threadDelay $ appliedDelay reconnectTryIndex
      pure $ M.singleton addrToReconnect $ Just ()

  let addressesChangeE = leftmost [addrAddE, addrDelE, addrReconnectE]
  nodeReqSelector <- getIndexReqSelector
  (addrE, _) <- getActivationEF
  activeConnectionsRef <- getActiveConnsRef
  seed <- mkResolvSeed
  activeNodesD <- listWithKeyShallowDiff initialAddressMap addressesChangeE $ \address _ _ -> do
    mAddr <- resolveAddr seed defIndexerPort address
    closedConnectionE <- case mAddr of
      Just addr -> do
        nodeLog $ "<" <> showt address <> ">: Connect"
        let reqE = select nodeReqSelector $ Const2 address
        conn <- initIndexerConnection address (namedAddrSock addr) reqE
        modifyExternalRef activeConnectionsRef $ (, ()) . M.insert address conn
        validVerE <- headE $ updated $ indexConIndexerVersion conn
        performEvent $ ffor validVerE $ const $
          modifyExternalRef addressReconnectionCountMapRef $ (, ()) . M.delete address
        indexerStatusUpdater conn
        -- Everything below this line is handling the closure of a connection
        -- the event the socket fires when it wants to be closed
        timeoutE <- connectionLatencyWidget conn
        let closedSocketE  = not . isCloseFinal <$> indexConClosedE conn
            closedConnectionE = leftmost [ closedSocketE, True <$ timeoutE]
        -- remove the connection from the connection map
        closedConnectionE' <- performEvent $ ffor closedConnectionE $ 
          ((modifyExternalRef activeConnectionsRef $ (, ()) . M.delete address) >>) . pure 
        -- send out the event to delete this widget
        pure closedConnectionE'
      _ -> (True <$) <$> getPostBuild
    pure $ (address,) <$> closedConnectionE
  pure ()
  where
    maxDelayExp = 7
    appliedDelay triesCount = 2 ^ (min triesCount maxDelayExp) * 1000000
    nodeLog t = logWrite $ "[indexerNodeController]: " <> t

connectionLatencyWidget :: forall t m . MonadClient t m => IndexerConnection t -> m (Event t ())
connectionLatencyWidget connection = mdo
  let startE = void $ ffilter id $ updated $ indexConIsUp connection
  pongE <- pingNode =<< delay latencyCheckInterval (leftmost [pingE, startE])
  (pingE, timeoutE) <- switchDyn2 <$> workflow (body pongE)
  pure timeoutE
  where
    body :: Event t () -> Workflow t m (Event t (), Event t ())
    body pongE =  Workflow $ do 
      timeoutE <- delay (latencyCheckInterval * 2)  =<< getPostBuild
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