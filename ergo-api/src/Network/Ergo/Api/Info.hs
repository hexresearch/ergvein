module Network.Ergo.Api.Info where

import Control.Lens ((^.))
import Control.Monad.Reader
import Data.Aeson
import Data.String.Interpolate (i)
import Data.Text
import Data.Word
import GHC.Generics
import Network.Ergo.Api.Client
import Ergvein.Interfaces.Ergo.It.Api.NodeApi

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
