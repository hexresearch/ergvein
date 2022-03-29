module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans.Except
import Data.ByteString.Builder
import Data.Serialize.Put (runPutLazy)
import Data.Time.Clock.POSIX
import Data.Word
import Ergvein.Index.Protocol.Deserialization
import Ergvein.Index.Protocol.Serialization
import Ergvein.Index.Protocol.Types
import Ergvein.Index.Protocol.Utils
import Ergvein.Index.Server.TCPService.Server
import Ergvein.Index.Server.TCPService.Socket (sendLazy)
import Ergvein.Index.Server.TCPService.Supervisor
import Foreign.C.Types
import Network.Socket hiding (recv)
import Network.Socket.ByteString  (recv)
import Options.Applicative
import System.Random

import qualified Control.Exception           as E
import qualified Data.ByteString             as BS
import qualified Data.Text.IO                as T
import qualified Data.Vector.Unboxed         as UV

filterBatchSize, filterStartHeight, filterEndHeight :: Word64
filterBatchSize   = 300
filterStartHeight = 400000
filterEndHeight   = 550000

data Command = CommandLoad Int String String

options :: Parser Command
options = subparser (command "listen" $ info (loadCmd <**> helper) $ progDesc "Start server")
  where
    loadCmd = CommandLoad
            <$> (read <$> strArgument ( metavar "CONFIG_N_CLIENTS" ))
            <*> strArgument ( metavar "CONFIG_ADDR" )
            <*> strArgument ( metavar "CONFIG_PORT" )

startServer :: Command -> IO ()
startServer cmd = case cmd of
  CommandLoad clientsNumber addr port -> do
    T.putStrLn "Server starting"
    withWorkersUnion $ \wrk -> do
      replicateM_ clientsNumber $ spawnWorker wrk $ forever $ runTCPClient addr port $ reportOccurringException . receiveFilters
      forever $ threadDelay maxBound
  where
    reportOccurringException :: IO () -> IO ()
    reportOccurringException = (`E.catch` (print . show :: E.SomeException -> IO ()))

receiveFilters :: Socket -> IO ()
receiveFilters s = do
  handshake
  forM_ [filterStartHeight, filterStartHeight + filterBatchSize .. filterEndHeight] recvByHeight
  T.putStrLn "Done full range filters receiving"
  where
    doSend = sendLazy s . runPutLazy . messageBuilder
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
    recvByHeight height = do
      doSend $ MFiltersRequest $ FilterRequest BTC height filterBatchSize
      Right (MessageHeader MFiltersResponseType filtersResponseLength) <- runExceptT $ messageHeader s
      filtersResponseBytes <- recvExact $ fromIntegral filtersResponseLength
      let Right (MFiltersResponse !_r) = parseTillEndOfInput (messageParser MFiltersResponseType) filtersResponseBytes
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
