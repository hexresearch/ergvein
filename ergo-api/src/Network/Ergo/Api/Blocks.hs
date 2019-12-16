module Network.Ergo.Api.Blocks where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.String.Interpolate (i)
import Data.Text
import Data.Word
import GHC.Generics
import Network.Ergo.Api.Client
import Ergvein.Interfaces.Ergo.Api

import Ergvein.Interfaces.Ergo.Header
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Interfaces.Ergo.Scorex.Util.Package

import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

basePrefix :: Text
basePrefix = "/blocks/"

getHeaderIdsAtHeight :: ApiMonad m => Height -> m [ModifierId]
getHeaderIdsAtHeight height = do
  client <- getClient
  let url = [i|#{ clientUrl client }#{ basePrefix }at/#{ unHeight height }|]
  response <- liftIO $ W.asJSON  =<< WS.getWith (clientOpts client) (clientSession client) url
  pure $ response ^. W.responseBody

getHeaderById ::  ApiMonad m => ModifierId -> m Header
getHeaderById id = do
  client <- getClient
  let url = [i|#{ clientUrl client }#{ basePrefix }#{ toHex $ unModifierId id }/header|]
  response <- liftIO $ W.asJSON =<< WS.getWith (clientOpts client) (clientSession client) url
  pure $ response ^. W.responseBody

getTransactionsById :: ApiMonad m => ModifierId -> m BlockTransactions
getTransactionsById id = do
  client <- getClient
  let url = [i|#{ clientUrl client }#{ basePrefix }#{ toHex $ unModifierId id }//blocks/{headerId}/transactions|]
  response <- liftIO $ W.asJSON =<< WS.getWith (clientOpts client) (clientSession client) url
  pure $ response ^. W.responseBody