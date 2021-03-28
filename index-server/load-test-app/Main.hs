module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.ByteString.Builder
import Data.Text (Text, pack)
import Data.Time.Clock.POSIX
import Data.Word
import Ergvein.Index.Protocol.Serialization
import Ergvein.Socket.Base (sendLazy)
import Foreign.C.Types
import Network.Socket hiding (recv)
import Network.Socket.ByteString  (recv)
import Options.Applicative
import System.Random
import Text.Read

import Ergvein.Index.Protocol.Types
import Ergvein.Socket.Supervisor
import Ergvein.Index.Protocol.Deserialization

import qualified Control.Exception          as E
import qualified Data.ByteString            as BS
import qualified Data.Text.IO               as T
import qualified Data.Vector.Unboxed        as UV
import qualified Data.Vector        as V

import Data.Attoparsec.ByteString hiding (Parser)
import Data.Either.Combinators

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

main' :: IO ()
main' = do
    startServer =<< execParser opts
    where
      opts = info (options <**> helper)
        ( fullDesc
       <> progDesc "Starts index server load test"
       <> header "ergvein-index-load-test")

main :: IO ()
main = do
  runTCPClient "0.0.0.0" "8667" $ \s -> do
    let doSend = sendLazy s . toLazyByteString . messageBuilder
    ver <- ownVersion
    doSend $ MVersion ver
    Right (MessageHeader MVersionACKType 0) <- runExceptT (messageHeader s)
    doSend $ MVersionACK VersionACK
    print =<< runExceptT (srvRequest s =<< messageHeader s)
    -- Request filters
    forM_ [400000] $ \h -> do
      print "send"
      doSend $ MFiltersRequest $ FilterRequest BTC h 10
      v <- runExceptT (messageHeader s)
      print v
      let Right (MessageHeader MFiltersResponseType n) = v
      print $ "Received " <> (show n) <> " bytes"
      bs <- recv s $ fromIntegral n
      when (BS.length bs /= fromIntegral n) $ error "OOPS"
      case parseMessage MFiltersResponseType bs of
        Left err -> print err
        Right (MFiltersResponse (FilterResponse cur v),_) -> print cur >> print (V.length v)

startServer :: Options -> IO ()
startServer Options{..} = case optsCommand of
  CommandLoad clnts addr port -> do
    T.putStrLn $ pack "Server starting"
    withWorkersUnion $ \wrk -> do
      replicateM_ clnts $ spawnWorker wrk $ forever $ runTCPClient addr port $ \s -> do
        let doSend = sendLazy s . toLazyByteString . messageBuilder
        ver <- ownVersion
        doSend $ MVersion ver
        Right (MessageHeader MVersionACKType 0) <- runExceptT (messageHeader s)
        doSend $ MVersionACK VersionACK
        -- print =<< runExceptT (srvRequest s =<< messageHeader s)
        -- Request filters
        forM_ [100000, 150000, 160000] $ \h -> do
          print "send"
          doSend $ MFiltersRequest $ FilterRequest BTC h 300
          Right (MessageHeader MFiltersResponseType n) <- runExceptT (messageHeader s)
          print $ "Received " <> (show $ 8 + n) <> " bytes"
          bs <- recv s (fromIntegral n)
          when (BS.length bs /= fromIntegral n) $ error "OOPS"
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

srvRequest :: MonadIO m => Socket -> MessageHeader -> ExceptT Reject m Message
srvRequest sock MessageHeader{..} = do
  messageBytes <- if not $ messageHasPayload msgType
    then pure mempty
    else liftIO $ recv sock $ fromIntegral msgSize
  except $ mapLeft (\_-> Reject msgType MessageParsing "Failed to parse message body") $ eitherResult $ parse (messageParser msgType) messageBytes


messageHeader :: MonadIO m => Socket -> ExceptT Reject m MessageHeader
messageHeader sock = do
  mid <- messageId =<< recvVarInt sock
  if not $ messageHasPayload mid then pure (MessageHeader mid 0) else do
    l <- messageLength =<< recvVarInt sock
    pure $ MessageHeader mid l

messageId :: Monad m => BS.ByteString -> ExceptT Reject m MessageType
messageId bs
  | BS.null bs = except $ Left $ Reject MVersionType ZeroBytesReceived "Expected bytes for message header (id)"
  | otherwise  = case eitherResult $ parse messageTypeParser bs of
      Left  _ -> throwE $ Reject MVersionType MessageHeaderParsing "Failed to parse header message id"
      Right m -> pure m


messageLength :: Monad m => BS.ByteString -> ExceptT Reject m Word32
messageLength bs
  | BS.null bs = except $ Left $ Reject MVersionType ZeroBytesReceived "Expected bytes for message header (length)"
  | otherwise  = case eitherResult $ parse messageLengthParser bs of
      Left  _ -> throwE $ Reject MVersionType MessageHeaderParsing "Failed to parse header message length"
      Right n -> pure n


recvVarInt :: MonadIO m => Socket -> m BS.ByteString
recvVarInt sock = liftIO $ do
  hbs <- recv sock 1
  if BS.null hbs then pure hbs else do
    let hb = BS.head hbs
    if | hb == 0xFF -> (hbs <>) <$> recv sock 8
       | hb == 0XFE -> (hbs <>) <$> recv sock 4
       | hb == 0xFD -> (hbs <>) <$> recv sock 2
       | otherwise -> pure hbs
