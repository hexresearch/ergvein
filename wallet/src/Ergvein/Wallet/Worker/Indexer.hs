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
import Ergvein.Wallet.Monad.Util

import Ergvein.Text
import Ergvein.Wallet.Monad.Client
import Ergvein.Wallet.Monad.Prim
import Ergvein.Wallet.Native
import Ergvein.Wallet.Indexer.Socket
import Ergvein.Wallet.Settings
import Control.Monad.IO.Class

import qualified Data.Map.Strict as M

connectionTimeout :: NominalDiffTime
connectionTimeout = 60

reconnectTimeout :: NominalDiffTime
reconnectTimeout = 5

idx :: (MonadIndexClient t m, MonadHasSettings t m) => m ()
idx =  mdo
  pure ()

indexerNodeController :: (MonadIndexClient t m, MonadHasSettings t m) => m ()
indexerNodeController  = mdo
  nodeLog "Starting"
  sel <- getIndexReqSelector
  (addrE, _) <- getActivationEF
  connRef <- getActiveConnsRef
  seed <- mkResolvSeed
  addrs <- M.filter _peerInfoIsActive . _settingsAddrs <$> (readExternalRef =<< getSettingsRef)
  let initMap = () <$ addrs
      closedE = switchDyn $ ffor valD $ leftmost . M.elems
      delE = (\u -> M.singleton u Nothing) <$> closedE
      addE = (\us -> M.fromList $ (, Just ()) <$> us) <$> addrE
      actE = leftmost [delE, addE] 
  valD <- listWithKeyShallowDiff initMap actE $ \n _ _ -> do
    mAddr <- parseSockAddrs seed [n]
    case mAddr of
      addr:_ ->  do
        nodeLog $ "<" <> n <> ">: Connect"
        let reqE = select sel $ Const2 n
        conn <- initIndexerConnection addr reqE
        modifyExternalRef connRef $ (, ()) . M.insert n conn
    
        -- Everything below this line is handling the closure of a connection
        -- the event the socket fires when it wants to be closed
        let closedE' = indexConClosedE conn
        failedToConnectE <- connectionWidget conn
        -- closedE'' -- init closure procedure here
        let closedE'' = leftmost [closedE', failedToConnectE]
        -- remove the connection from the connection map
        closedE''' <- performEvent $ ffor closedE'' $ const $ modifyExternalRef connRef $ \cm -> (M.delete n cm, ())
        -- send out the event to delete this widget
        pure $ n <$ closedE'''
      _-> pure never
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
