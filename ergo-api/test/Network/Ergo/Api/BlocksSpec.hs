module Network.Ergo.Api.BlocksSpec where

import Control.Monad.Reader
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Network.Ergo.Api.Blocks
import Network.Ergo.Api.Client
import Network.Ergo.Api.TestUtil
import Test.Hspec

spec :: Spec
spec = do
  describe "when testing Blocks functions" $ do
    it "can request block headers at height" $ do
      client <- testClient
      idsAtHeight <- flip runReaderT client $ getHeaderIdsAtHeight $ Height 1
      null idsAtHeight `shouldBe` False

    it "can request block header by id" $ do
      client <- testClient
      idsAtHeight <- flip runReaderT client $ getHeaderIdsAtHeight $ Height 1
      let mainChainId = head idsAtHeight
      header <- flip runReaderT client $ getHeaderById mainChainId
      True  `shouldBe` True

    it "can request block transactions by id" $ do
      client <- testClient
      idsAtHeight <- flip runReaderT client $ getHeaderIdsAtHeight $ Height 1
      let mainChainId = head idsAtHeight
      header <- flip runReaderT client $ getTransactionsById mainChainId
      True  `shouldBe` True

    it "can request full block by id" $ do
      client <- testClient
      idsAtHeight <- flip runReaderT client $ getHeaderIdsAtHeight $ Height 1
      let mainChainId = head idsAtHeight
      header <- flip runReaderT client $ getById mainChainId
      True  `shouldBe` True