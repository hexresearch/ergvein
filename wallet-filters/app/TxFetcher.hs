module Main where

import Control.Monad.IO.Class
import Data.Default
import Data.Maybe
import Data.Text (Text, unpack)
import Ergvein.Filters.Btc
import GHC.Generics
import Network.Bitcoin.Api.Blockchain
import Network.Bitcoin.Api.Client
import Options.Generic

data Options = Options {
  nodeAddress  :: Maybe Text <?> "Address of node"
, nodePort     :: Maybe Int <?> "Port of node"
, testnet      :: Bool <?> "Is this testnet network"
, nodeLogin    :: Text <?> "Login for node RPC"
, nodePassword :: Text <?> "Password for node RPC" -- TODO: take password more securely
, nodeBlock    :: Integer <?> "Which block to fetch"
} deriving (Generic)

instance ParseRecord Options

getNodeAddress :: Options -> Text
getNodeAddress Options{..} = fromMaybe "127.0.0.1" $ unHelpful nodeAddress

getNodePort :: Options -> Int
getNodePort Options{..} = fromMaybe (if unHelpful testnet then 18332 else 8332) $ unHelpful nodePort

nodeCall :: MonadIO m => Options -> (Client -> IO a) -> m a
nodeCall opts = liftIO . withClient
 (unpack $ getNodeAddress opts)
 (getNodePort opts)
 (unHelpful $ nodeLogin opts)
 (unHelpful $ nodePassword opts)

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergvein transactions fetcher"
  let blockNum = unHelpful nodeBlock
  putStrLn $ "Getting block with height " <> show blockNum
  hash <- nodeCall opts $ \c -> getBlockHash c blockNum
  putStrLn $ "Block hash is" <> show hash
