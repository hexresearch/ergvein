module Network.Ergo.Api.BlocksSpec where

import           Test.Hspec
import Network.Ergo.Api.Client
import Network.Ergo.Api.Blocks
import qualified Data.Text                                    as T (pack)
import qualified Data.Text.IO as T


spec :: Spec
spec = do
  describe "fdfdfd" $ do
    it "blocks" $ do
      client <- newClient "127.0.0.1" 9052
      r <- getAtBlockHeight client 6
      T.putStrLn $ T.pack $ show r
      True `shouldBe` True