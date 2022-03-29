{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Ref
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import Data.String 
import Data.Text
import Ergvein.Core.Node
import Ergvein.Core.Settings
import Ergvein.Node.Resolve
import Network.DNS.Resolver
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Network.Haskoin.Network
import Network.Socket
import Options.Generic
import Reflex
import Reflex.Host.Class
import Reflex.Spider.Internal
import Reflex.Localize.Language
import Sepulcas.Native
import Sepulcas.Desktop.Native
import Ergvein.Aeson
import GHC.Generics (Generic)
import Reflex.Localize
import Reflex.Localize.Language

import qualified Control.Monad.Fail  as F
import qualified Data.Vector         as V
import qualified Reflex.Profiled     as RP

type EventChannel = Chan [DSum (EventTriggerRef Spider) TriggerInvocation]

-- | Languages that are supported by wallet
data instance Language
  = English
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

$(deriveJSON aesonOptions 'English)

data Options = Options {
  nodeAddress  :: Maybe String <?> "Address of node"
, nodePort     :: Maybe Int <?> "Port of node"
} deriving (Generic)

instance ParseRecord Options

getNodeAddress :: Options -> String
getNodeAddress Options{..} = fromMaybe "127.0.0.1" $ unHelpful nodeAddress

getNodePort :: Options -> PortNumber
getNodePort Options{..} = fromIntegral $ fromMaybe 9030 $ unHelpful nodePort

main :: IO ()
main = do
  opts@Options{..} <- getRecord "Ergvein protocol client example"
  let settings = defaultSettings English "."
  rs <- makeResolvSeed defaultResolvConf
  Just addr <- resolveAddr rs (getNodePort opts) (pack $ getNodeAddress opts) 
  
  (runSpiderHost :: SpiderHost Global a -> IO a) $ do
    events <- liftIO newChan
    ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      let testRunner = do 
            senv <- newSettingsEnv settings
            runSettings senv $ test $ namedAddrSock addr
      result <- runPostBuildT (runTriggerEventT testRunner events) postBuild
      pure (result, postBuildTriggerRef)
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ pure ()
    liftIO $ do
      processAsyncEvents events fc
      forever $ threadDelay maxBound
  where 
    test :: MonadSettings t m => SockAddr -> m (NodeBtc t)
    test addr = do
      (msgE, msgFire) <- newTriggerEvent
      let btcRating = 100 -- Initial rating for the node. 100 is a perfect node
      btcNode <- initBtcNode True btcRating addr msgE

      --handshake
      handshakeE <- delay 0.5 . ffilter id . updated . nodeconIsUp $ btcNode
      
      --blockHeader request test
      let requiredBlock :: BlockHash = fromString "000000000000000000053331707fcbaa576b72ce41cf3f82cb22010886e9fd9c"

      performEvent_ $ ffor handshakeE $ const $ liftIO $ do
        liftIO $ putStrLn $ "Sent request for block " ++ show requiredBlock
        msgFire $ NodeMsgReq $ NodeReqBtc $ MGetData $ GetData $ [InvVector InvBlock (getBlockHash requiredBlock)]
      
      let respE = fforMaybe (nodeconRespE btcNode) $ \case
            MBlock blk -> let
              bh = headerHash $ blockHeader blk
              in if bh == requiredBlock
                    then Just ()
                    else Nothing
            _ -> Nothing

      performEvent $ ffor respE $ const $ liftIO $ print "Test successful"

      pure btcNode

processAsyncEvents :: EventChannel -> FireCommand Spider (SpiderHost Global) -> IO ()
processAsyncEvents events (FireCommand fire) = void $ forkIO $ forever $ do
  ers <- readChan events
  _ <- runSpiderHost $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      pure $ (:=> Identity a) <$> me
    _ <- fire (catMaybes mes) $ pure ()
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  pure ()

instance F.MonadFail (SpiderHostFrame Global) where
  fail = liftIO . F.fail

instance F.MonadFail (RP.ProfiledM (SpiderHostFrame Global)) where
  fail = liftIO . F.fail
