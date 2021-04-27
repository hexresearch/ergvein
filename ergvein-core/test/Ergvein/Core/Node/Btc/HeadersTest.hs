module Ergvein.Core.Node.Btc.HeadersTest where

import Data.Text (Text)
import Data.Time
import Ergvein.Core.Node.Btc.Headers
import Ergvein.Text
import Network.Haskoin.Block
import Test.Tasty.Hspec

import qualified Data.Serialize                as S
import qualified Data.Text                     as T

unit_emptyTree :: IO ()
unit_emptyTree = getBestHeight (newHeadersTree 4242) `shouldBe` 4242

unit_singletonTree :: IO ()
unit_singletonTree = do
  let startH = 680690
  let tree = newHeadersTree startH
  t <- getCurrentTime
  tree1 <- either fail pure $ addHeader t header1 tree
  getBestHeight tree1 `shouldBe` startH+1

unit_simpleChain :: IO ()
unit_simpleChain = do
  let startH = 680690
  let tree = newHeadersTree startH
  t <- getCurrentTime
  tree1 <- either fail pure $ addHeader t header1 tree
  tree2 <- either fail pure $ addHeader t header2 tree1
  getBestHeight tree2 `shouldBe` startH+2

unit_simpleFork :: IO ()
unit_simpleFork = do
  let startH = 680690
  let tree = newHeadersTree startH
  t <- getCurrentTime
  tree1 <- either fail pure $ addHeader t header3 tree
  getBestHeight tree1 `shouldBe` startH+1
  tree2 <- either fail pure $ addHeader t header1 tree1
  tree3 <- either fail pure $ addHeader t header2 tree2
  getBestHeight tree3 `shouldBe` startH+2

-- 680690
header1 :: BlockHeader
header1 = loadBlockHeader "000080202fb7d954e8c3ce3456ebe46ffeddbe8727bce73bb38d080000000000000000005b3a706c9340f4c8152d71bafcb66e4957ccdbc49eee55130011d689add33d933ca1866093ef0b17ab70356b"

-- 680691
header2 :: BlockHeader
header2 = loadBlockHeader "00002020395e2592bca7de7d2767575efdf88287278770b4f63e06000000000000000000c3aec154b02f4e619de4b417ae032f39dcb5c7ab1e4ab23123833c81de526aaf97a7866093ef0b1738b12193"

-- 653181
header3 :: BlockHeader
header3 = loadBlockHeader "00004020bd6a1b8f07bb5ad25670397da324a7879ed5e45e613f040000000000000000006abe09c7aa44e9e7e9cefe1cec659aa22dd021e2770c993bbf7fef0edde212e4ba088b5fde950e17357f1dbd"

-- 653182
header4 :: BlockHeader
header4 = loadBlockHeader "00000020a9229060ea10ac054084bb7c599978495ddff21fa4d30c00000000000000000049fe90fe1a7bdda0f011824105af94f2a2ca4e462fb96f08ee4ba5221dbbecbc760a8b5fde950e1708d36e40"

loadBlockHeader :: Text -> BlockHeader
loadBlockHeader = either error id . S.decode @BlockHeader . hex2bs . T.filter (/= '\n')
