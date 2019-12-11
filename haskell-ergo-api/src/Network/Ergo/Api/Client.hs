module Network.Ergo.Api.Client (Client (..) , newClient ) where

import           Control.Lens              ((&), (?~))
import qualified Network.Wreq              as W
import qualified Network.Wreq.Session      as WS

import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE

-- | Client session data
data Client = Client {
  clientUrl     :: String,    -- ^ The JSON RPC url
  clientOpts    :: W.Options, -- ^ Default HTTP options to use with `wreq` requests
  clientSession :: WS.Session -- ^ Connection reuse of our HTTP session
  } deriving ( Show )

-- | Initializes a client and prepares it for making requests against the
--   Ergo API. Connection reuse is provided, and cleanup of any acquired
--   resources is handled automatically.
newClient :: String -> Int -> IO Client 
newClient host port = do
  let options :: W.Options
      options = W.defaults
      
      generateUrl :: String
      generateUrl = "http://" ++ host ++ ":" ++ show port 
  session <- WS.newAPISession
  pure $ Client generateUrl options session