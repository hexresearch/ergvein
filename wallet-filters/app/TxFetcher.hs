module Main where

import Control.Monad.IO.Class
import Data.Default
import Data.Foldable (traverse_)
import Data.Maybe
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding
import Ergvein.Filters.Btc
import Ergvein.Text
import GHC.Generics
import Network.Bitcoin.Api.Client
import Network.Haskoin.Block
import Network.Haskoin.Transaction
import Options.Generic
import System.IO

import qualified Data.HashMap.Strict as HM
import qualified Data.HexString as HS
import qualified Data.Serialize as S
import qualified Network.Bitcoin.Api.Blockchain as API

data Options = Options {
  nodeAddress  :: Maybe Text <?> "Address of node"
, nodePort     :: Maybe Int <?> "Port of node"
, testnet      :: Bool <?> "Is this testnet network"
, nodeLogin    :: Text <?> "Login for node RPC"
, nodePassword :: Text <?> "Password for node RPC" -- TODO: take password more securely
, nodeBlock    :: Integer <?> "Which block to fetch"
, outputFile   :: FilePath <?> "Where to write down resulted transactions"
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

blockInputTxs :: Block -> [TxHash]
blockInputTxs = map (outPointHash . prevOutput) . concatMap txIn . blockTxns

uniqTxs :: [TxHash] -> [TxHash]
uniqTxs = HM.elems . HM.fromList . fmap (\tx -> (txHashToHex tx, tx))

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergvein transactions fetcher"
  let blockNum = unHelpful nodeBlock
  putStrLn $ "Getting block with height " <> show blockNum
  hash <- nodeCall opts $ \c -> API.getBlockHash c blockNum
  putStrLn $ "Block hash is" <> show hash
  putStrLn "Getting block"
  mblockRaw <- nodeCall opts $ \c -> API.getBlockRaw c hash
  block <- maybe (fail "Cannot find block!") (pure . either error id . S.decode @Block . hex2bs . HS.toText) mblockRaw
  let txIds = uniqTxs $ drop 1 $ blockInputTxs block -- dropping coinbase
  putStrLn $ "Block downloaded. Block has " <> show (length txIds) <> " input transactions."
  txs <- traverse (fetchTx opts) txIds
  putStrLn $ "Writing to " <> unHelpful outputFile
  withFile (unHelpful outputFile) WriteMode $ \h -> traverse_ (writeTx h) txs
  putStrLn "Done"

fetchTx :: Options -> TxHash -> IO Tx
fetchTx opts h = do
  putStrLn $ "Fetching tx " <> (unpack . txHashToHex) h
  mtx <- nodeCall opts $ \c -> API.getRawTransaction c (HS.hexString . encodeUtf8 . txHashToHex $ h)
  maybe (fail "Cannot find tx!") (pure . either error id . S.decode @Tx . hex2bs . HS.toText) mtx

writeTx :: Handle -> Tx -> IO ()
writeTx h tx = hPutStrLn h $ unpack . bs2Hex $ S.encode tx
