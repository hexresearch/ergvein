module Network.Ergo.Api.Utxo where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.String.Interpolate (i)
import Data.Text
import Network.Ergo.Api.Client
import Ergvein.Interfaces.Ergo.Api

import Ergvein.Interfaces.Ergo.Scorex.Util.Package

import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

basePrefix :: Text
basePrefix = "/utxo/"

getById :: ApiMonad m => TransactionBoxId -> m ErgoTransactionOutput
getById txBoxId = do
  client <- getClient
  let url = [i|#{ clientUrl client }#{ basePrefix }#{ toHex $ unTransactionBoxId txBoxId }|]
  response <- liftIO $ W.asJSON =<< WS.getWith (clientOpts client) (clientSession client) url
  pure $ response ^. W.responseBody
