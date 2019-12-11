module Network.Ergo.Api.Blocks where

import Network.Ergo.Api.Client
import           Control.Lens              ((^.))
import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS
import Data.Word
import Ergvein.Interfaces.Ergo.Header
import GHC.Generics
import Data.Aeson
import Control.Monad.Reader

basePrefix = "/blocks/"

getHeaderIdsAtHeight :: ApiMonad m => Word32 -> m [String]
getHeaderIdsAtHeight height = do
  client <- getClient
  let url = clientUrl client ++ basePrefix ++ "at/" ++ show height
  response <- liftIO $ W.asJSON  =<< WS.getWith (clientOpts client) (clientSession client) url
  let parsed = show $ response
  pure $ response ^. W.responseBody

getHeaderById ::  ApiMonad m => String -> m String
getHeaderById id = do
    client <- getClient
    let url = clientUrl client ++ basePrefix ++ id ++ "/header"
    response <- liftIO $ WS.getWith (clientOpts client) (clientSession client) url
    let parsed = show $ response ^. W.responseBody
    pure $ parsed