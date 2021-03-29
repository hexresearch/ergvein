module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Except
import Data.Function
import Data.Text (Text, pack)
import Data.Word
import Options.Applicative
import System.IO
import Text.Read
import System.Random
import Data.Time.Clock.POSIX
import Foreign.C.Types
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString  (recv, sendAll)
import Ergvein.Index.Protocol.Serialization
import Data.ByteString.Builder
import Ergvein.Index.Server.TCPService.Socket (sendLazy)

import Ergvein.Index.Server.BlockchainScanning.Bitcoin
import Ergvein.Index.Server.BlockchainScanning.BitcoinApiMonad
import Ergvein.Index.Server.Config
import Ergvein.Index.Server.TCPService.BTC
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.TCPService.Server as Srv
import Ergvein.Index.Server.TCPService.Supervisor

import qualified Data.Text.IO                as T
import qualified Network.Bitcoin.Api.Client  as BitcoinApi
import qualified Network.Haskoin.Constants   as HK
import qualified Data.Vector.Unboxed  as UV
import qualified Data.ByteString as BS

data Options = Options {
  -- | Which command to execute
    optsCommand           :: Command
  -- |Def: False. Start only BC-scanning threads
}

wordReader :: ReadM Word64
wordReader = eitherReader $ \arg -> case readMaybe arg of
  Nothing -> Left ("Cannot parse word: " ++ arg)
  Just w  -> Right w

type ServerUrl = Text

data Command
  = CommandLoad Int String String

options :: Parser Options
options = Options
  <$> subparser (command "listen" $ info (loadCmd <**> helper) $ progDesc "Start server")
  where
    loadCmd = CommandLoad
            <$> (read <$> strArgument ( metavar "CONFIG_N_CLIENTS" ))
            <*> strArgument ( metavar "CONFIG_ADDR" )
            <*> strArgument ( metavar "CONFIG_PORT" )

main :: IO ()
main = do
    startServer =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
       <> progDesc "Starts index server load test"
       <> header "ergvein-index-load-test")

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
  CommandLoad clnts addr port -> do
    T.putStrLn $ pack "Server starting"
    withWorkersUnion $ \wrk -> do
      replicateM_ clnts $ spawnWorker wrk $ forever $ runTCPClient addr port $ \s -> do
        print $ show "reeeee"
        let doSend = sendLazy s . toLazyByteString . messageBuilder
        ver <- ownVersion
        doSend $ MVersion ver
        Right (MessageHeader MVersionType verMsgSize) <- runExceptT (messageHeader s)
        recv s $ fromIntegral verMsgSize
        Right (MessageHeader MVersionACKType 0) <- runExceptT (messageHeader s)
        doSend $ MVersionACK VersionACK
        -- Request filters
        forM_ [400000, 400300 .. 550000] $ \h -> do
          doSend $ MFiltersRequest $ FilterRequest BTC h 300
          Right (MessageHeader MFiltersResponseType n) <- runExceptT (messageHeader s)
          bs <- recv s (fromIntegral n)
          when (BS.length bs /= fromIntegral n) $ (print $ show $ BS.length bs) <> (print $ show n) <> error "FAIL"
          print "OK"
        -- print =<< runExceptT (messageHeader s)
        --sendLazy s . toLazyByteString . messageBuilder . MFiltersRequest $ request 
        -- we need to fetch actual filters size, because dumb reading till cause waiting and long timeout 
        --tillEnd
        print "done"
      forever $ threadDelay maxBound

    

ownVersion :: IO Version
ownVersion = do
  nonce <- randomIO
  time  <- CTime . floor <$> getPOSIXTime
  let scanNfo = UV.empty
  pure $ Version {
      versionVersion    = protocolVersion
    , versionTime       = time
    , versionNonce      = nonce
    , versionScanBlocks = scanNfo
    }

request = FilterRequest {
          filterRequestMsgCurrency = BTC
        , filterRequestMsgStart = 445123
        , filterRequestMsgAmount = 2000
        }

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
    openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
