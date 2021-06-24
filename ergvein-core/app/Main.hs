{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Ref
import Data.Dependent.Sum (DSum (..))
import Data.Ergo.Modifier
import Data.Ergo.Protocol
import Data.Functor.Identity
import Data.IORef
import Data.Maybe
import Ergvein.Core.Node.Ergo
import Ergvein.Core.Node.Types
import Network.Socket
import Options.Generic
import Reflex
import Reflex.Host.Class
import Reflex.Spider.Internal

import qualified Control.Monad.Fail  as F
import qualified Data.Vector         as V
import qualified Reflex.Profiled     as RP

type EventChannel = Chan [DSum (EventTriggerRef Spider) TriggerInvocation]

addr = SockAddrInet 9030 $ tupleToHostAddress (127,0,0,1)

main :: IO ()
main = (runSpiderHost :: SpiderHost Global a -> IO a) $ do
  events <- liftIO newChan
  ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    result <- runPostBuildT (runTriggerEventT test events) postBuild
    pure (result, postBuildTriggerRef)
  mPostBuildTrigger <- readRef postBuildTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ pure ()
  liftIO $ do
    processAsyncEvents events fc
    forever $ threadDelay maxBound
  where 
    test = do
      (msgE, msgFire) <- newTriggerEvent
      ergoNode <- f addr msgE never

      --handshake
      let handshakeE = fforMaybe (nodeconRespE ergoNode) $ \case
            MsgHandshake _-> Just ()
            _-> Nothing
      
      --blockHeader request test
      let requiredBlock = "81a93bb7eb27bfb84b7afc6b64c75ee54023bb21224125214af218ddc41d60ec"

      performEvent_ $ ffor handshakeE $ const $ liftIO $
        msgFire $ NodeMsgReq $ NodeReqErgo $ MsgOther $ MsgRequestModifier $ RequestModifierMsg ModifierBlockHeader [requiredBlock]
      
      let respE = fforMaybe (nodeconRespE ergoNode) $ \case
            MsgOther (MsgModifier (ModifierMsg ModifierBlockHeader ids)) | isJust $ V.find ((== requiredBlock) . modifierId) ids -> Just ()
            _-> Nothing
    
      performEvent $ ffor respE $ const $ liftIO $ print "Test successful"

      pure ergoNode

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
