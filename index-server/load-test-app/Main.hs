module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString.Builder
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX
import Data.Word
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Utils
import Ergvein.Index.Server.TCPService.Socket (sendLazy)
import Foreign.C.Types
import Network.Socket hiding (recv)
import Network.Socket.ByteString  (recv, sendAll)
import Options.Applicative
import System.Random
import Text.Read

import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Server.TCPService.Server
import Ergvein.Index.Server.TCPService.Supervisor
import Ergvein.Text

import qualified Control.Exception           as E
import qualified Data.ByteString             as BS
import qualified Data.Text.IO                as T
import qualified Data.Vector.Unboxed         as UV

filterBatchSize, filterStartHeight, filterEndHeight :: Word64
filterBatchSize   = 300
filterStartHeight = 400000
filterEndHeight   = 550000

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

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
  CommandLoad clientsNumber addr port -> do
    T.putStrLn $ pack "Server starting"
    withWorkersUnion $ \wrk -> do
      replicateM_ clientsNumber $ spawnWorker wrk $ forever $ runTCPClient addr port $ onException . receiveFilters 
      forever $ threadDelay maxBound
  where
    onException = (`E.catch` (print . show :: E.SomeException -> IO ()))

receiveFilters s = do
  handshake
  forM_ [filterStartHeight, (filterStartHeight + filterBatchSize) .. filterEndHeight] receiveFilters
  print "Done full range filters receiving"
  where
    doSend = sendLazy s . toLazyByteString . messageBuilder
    recvExact n = do
      bs <- recv s n
      let bsL = BS.length bs
      if bsL == n then pure bs else (bs <>) <$> recvExact (n - bsL)
    handshake = do
      ver <- ownVersion
      doSend $ MVersion ver
      doSend $ MVersionACK VersionACK
      Right versionHeader@(MessageHeader MVersionType _) <- runExceptT $ messageHeader s
      Right (MVersion _) <- runExceptT $ request s versionHeader
      Right (MessageHeader MVersionACKType 0) <- runExceptT $ messageHeader s
      pure ()
    receiveFilters height = do
      doSend $ MFiltersRequest $ FilterRequest BTC height filterBatchSize
      Right (MessageHeader MFiltersResponseType filtersResponseLength) <- runExceptT $ messageHeader s
      filtersResponseBytes <- recvExact $ fromIntegral filtersResponseLength
      let Right (MFiltersResponse !_) = parseTillEndOfInput (messageParser MFiltersResponseType) filtersResponseBytes
      pure ()

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

main :: IO ()
main = do
    startServer =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
       <> progDesc "Starts index server load test"
       <> header "ergvein-index-load-test")