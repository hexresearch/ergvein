{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Tests.Ergo.ValidateChain where

import Test.Tasty.Hspec

import Control.Monad
import Data.Either.Combinators
import Data.Function
import Data.Functor
import Data.String
import Data.Text (Text)

import Ergvein.Interfaces.Ergo.Modifiers.History.ValidateChain
import Ergvein.Interfaces.Ergo.Scorex.Core.Block
import Ergvein.Interfaces.Ergo.Scorex.Util.Package


-- |  G                                               4
-- |  .-----------------------c---------------------  3
-- |  .-----------a-----------*-----------f---------  2
-- |  .-----.-----*-----b-----*-----d-----*-----h---  1
-- |  .--.--.--.--.--.--.--.--.--.--*--e--*--g--*--j  0

type PoPowTestHeader = PoPowNormHeaderF Text Int

instance HasGetHeight Int where
  getHeight = Height . fromIntegral

chain1 :: [PoPowTestHeader]
genesis :: PoPowTestHeader
(genesis:(reverse -> chain1)) = [
    ("G", 4, "")
  , ("a", 2, "  G")
  , ("b", 1, " aaG")
  , ("c", 3, " baG")
  , ("d", 1, " cccG")
  , ("e", 0, " dccG")
  , ("f", 2, "edccG")
  , ("g", 0, "fffcG")
  , ("h", 1, "gffcG")
  , ("j", 0, "hhfcG")
  ] & (zip [0..]) <&> \(n, (a, lev, links)) ->
      PoPowNormHeader {
        blockId = a
      , level   = lev
      , hdr     = n
      , ilinks  = reverse $ fromString . (:[]) <$> links
      }

-- PoPowNormHeader {blockId = "j", level = 0, hdr = 9, ilinks = ["G","c","f","h","h"]}
-- PoPowNormHeader {blockId = "h", level = 1, hdr = 8, ilinks = ["G","c","f","f","g"]}
-- PoPowNormHeader {blockId = "g", level = 0, hdr = 7, ilinks = ["G","c","f","f","f"]}
-- PoPowNormHeader {blockId = "f", level = 2, hdr = 6, ilinks = ["G","c","c","d","e"]}
-- PoPowNormHeader {blockId = "e", level = 0, hdr = 5, ilinks = ["G","c","c","d"," "]}
-- PoPowNormHeader {blockId = "d", level = 1, hdr = 4, ilinks = ["G","c","c","c"," "]}
-- PoPowNormHeader {blockId = "c", level = 3, hdr = 3, ilinks = ["G","a","b"," "]}
-- PoPowNormHeader {blockId = "b", level = 1, hdr = 2, ilinks = ["G","a","a"," "]}
-- PoPowNormHeader {blockId = "a", level = 2, hdr = 1, ilinks = ["G"," "," "]}

spec_isValidChainAnchoredTo :: Spec
spec_isValidChainAnchoredTo = do
  it "tests isValidChainAnchoredTo'" $ do
    -- putStrLn ""
    -- mapM_ print chain1
    isValidChainAnchoredTo' genesis chain1 `shouldBe` Right ()

spec_isValidEmptyChain :: Spec
spec_isValidEmptyChain = do
  it "tests isValidChainAnchoredTo' on empty chain" $ do
    isValidChainAnchoredTo' genesis mempty `shouldBe` Right ()

spec_isNotValidChainAnchoredTo :: Spec
spec_isNotValidChainAnchoredTo = do
  it "tests isValidChainAnchoredTo'" $ do
    let chain' = reverse $ zip [0..] chain1 <&> \(n,h) -> h { hdr = n }
    isValidChainAnchoredTo' genesis chain' `shouldBe` (Left "Interlinks is not valid for block \"h\"")
