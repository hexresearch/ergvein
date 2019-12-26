module Data.Encoding.GolombRiceTest where

import qualified Data.Bitstream                as BS
import           Data.Encoding.GolombRice.Strict.Internal
                                               as G
import           Data.Foldable
import           Data.Word
import           Test.Tasty.Hspec
import           Test.Tasty.QuickCheck

p :: Int
p = 19

type GRWord = G.GolombRice Word64

spec_basicEncoding :: Spec
spec_basicEncoding = describe "basic test vectors" $ do
  it "empty stream is empty" $ G.null (G.empty p :: GRWord) `shouldBe` True
  it "non empty stream is not empty"
    $          G.null (G.singleton p 0 :: GRWord)
    `shouldBe` False

spec_encodingCheck :: Spec
spec_encodingCheck = describe "encoded bits are correct" $ do
  let showBits = fmap $ \v -> if v then '1' else '0'
  let check i is =
        it ("encodes " <> show i <> " as " <> showBits is)
          $ let s = BS.unpack $ encodeWord 2 i in s `shouldBe` is
  traverse_
    (uncurry check)
    [ (0 , [False, False, False])
    , (1 , [False, False, True])
    , (2 , [False, True, False])
    , (3 , [False, True, True])
    , (4 , [True, False, False, False])
    , (5 , [True, False, False, True])
    , (6 , [True, False, True, False])
    , (7 , [True, False, True, True])
    , (8 , [True, True, False, False, False])
    , (9 , [True, True, False, False, True])
    , (10, [True, True, False, True, False])
    ]

prop_encodingDecodingWord :: Small Word64 -> Bool
prop_encodingDecodingWord (Small w) =
  let (rw, s) = decodeWord p (encodeWord p w) in rw == w && BS.null s
