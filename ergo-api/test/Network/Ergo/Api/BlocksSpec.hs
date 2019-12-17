module Network.Ergo.Api.BlocksSpec where

import           Test.Hspec
import Network.Ergo.Api.Client
import Network.Ergo.Api.Blocks
import qualified Data.Text                                    as T (pack)
import qualified Data.Text.IO as T
import qualified Network.Wreq              as W
import           Control.Lens              ((^.))
import qualified Data.ByteString.Lazy      as BL
import Control.Monad.Reader
import Ergvein.Interfaces.Ergo.Scorex.Util.Package
import Data.ByteString (unpack)
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Network.Ergo.Api.Info


spec :: Spec
spec = do
  describe "Blocks api" $ do
    it "blocks" $ do

      client <- newClient "127.0.0.1" 9052
      r <- flip runReaderT client $ getHeaderIdsAtHeight $ Height 6
      r3 <- flip runReaderT client $ getById $ head r
      True `shouldBe` True