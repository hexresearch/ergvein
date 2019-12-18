module Network.Ergo.Api.Utxo where

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
basePrefix = "/utxo/"

getById :: ApiMonad m => TransactionBoxId -> m String
getById id = do
  client <- getClient
  let url = [i|#{ clientUrl client }#{ basePrefix }#{ toHex $ unTransactionBoxId id }|]
  response <- liftIO $ WS.getWith (clientOpts client) (clientSession client) url
  pure $ show $ response ^. W.responseBody