module Network.Ergo.Api.Blocks where

import Network.Ergo.Api.Client
import           Control.Lens              ((^.))
import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS
import Data.Word

basePrefix = "/blocks"
getAtBlockHeight:: Client -> Word32 -> IO String
getAtBlockHeight client height = do
    let url = clientUrl client ++ basePrefix ++ "/at/" ++ show height
    response <- WS.getWith (clientOpts client) (clientSession client) url
    let parsed = show $ response ^. W.responseBody 
    pure $ parsed