module Network.Ergo.Api.InfoSpec where

import Control.Monad.Reader
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Network.Ergo.Api.Info
import Network.Ergo.Api.Client
import Network.Ergo.Api.TestUtil
import Test.Hspec

spec :: Spec
spec = do
  describe "when testing Info functions" $ do
    it "can request node info" $ do
        client <- testClient
        info <- flip runReaderT client $ getInfo
        True  `shouldBe` True