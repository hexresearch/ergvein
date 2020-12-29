module Network.Ergo.Api.Info where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.String.Interpolate (i)
import Data.Text
import Ergvein.Interfaces.Ergo.It.Api.NodeApi
import Network.Ergo.Api.Client

import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

basePrefix :: Text
basePrefix = "/info"

getInfo ::  ApiMonad m => m NodeInfo
getInfo = do
  client <- getClient
  let url = [i|#{ clientUrl client }#{ basePrefix }|]
  response <- liftIO $ W.asJSON =<< WS.getWith (clientOpts client) (clientSession client) url
  pure $ response ^. W.responseBody
