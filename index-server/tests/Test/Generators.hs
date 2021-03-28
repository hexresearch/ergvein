module Test.Generators where

import Control.Monad (replicateM)
import Data.Serialize (decode)
import Network.Haskoin.Transaction hiding (TxHash)
import qualified Network.Haskoin.Transaction as HK
import Test.QuickCheck
import Test.QuickCheck.Instances


import Ergvein.Index.Server.Types
import Ergvein.Index.Server.PeerDiscovery.Types
import Ergvein.Types.Transaction

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as BB
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Short      as BSS
import qualified Data.List                  as L
import qualified Data.Vector                as V
import qualified Data.Vector.Unboxed        as UV

--------------------------------------------------------------------------
-- generators

getRandBounded :: (Enum a, Bounded a) => Gen a
getRandBounded = oneof $ pure <$> [minBound .. maxBound]

getRandBoundedExcluding :: (Eq a, Enum a, Bounded a) => [a] -> Gen a
getRandBoundedExcluding exs = oneof $ fmap pure $ filter (\e -> not $ e `elem` exs) $ [minBound .. maxBound]

arbitraryBS32 = fmap BS.pack $ replicateM 32 arbitrary

arbitraryBSS32 = do
  a <- arbitrary
  pure $ BSS.pack $ if BSS.length a < 32
    then [0..31]
    else take 32 . BSS.unpack $ a

-- instance Arbitrary TxHash where
--   arbitrary = oneof [
--       BtcTxHash <$> arbitrary
--     -- , ErgTxHash . ErgTxId <$> arbitraryBSS32 -- We do not support ERGO at the moment
--     ]

instance Arbitrary HK.TxHash where
  arbitrary = do
    bs <- arbitraryBS32
    either (const error "Wut") pure $ decode bs

instance Arbitrary OutPoint where
  arbitrary = OutPoint <$> arbitrary <*> arbitrary

instance Arbitrary TxIn where
  arbitrary = TxIn <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TxOut where
  arbitrary = TxOut <$> arbitrary <*> arbitrary

instance Arbitrary Tx where
  arbitrary = do
    b <- arbitrary
    is <- arbitrary
    let genWit = if b then pure [] else fmap unpackWits $ replicateM (length is) $ arbitrary
    Tx 1 is <$> arbitrary <*> genWit <*> arbitrary
    -- Tx <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--------------------------------------------------------------------------
-- misc

newtype WitItem = WitItem {getWitItem :: BS.ByteString}

instance Arbitrary WitItem where
  arbitrary = WitItem <$> arbitraryBS32

newtype WitStack = WitStack {getWitStack :: [WitItem]}

instance Arbitrary WitStack where
  arbitrary = do
    a <- arbitrary
    pure $ WitStack $ case a of
      [] -> [WitItem $ BS.pack [0..31]]
      _ -> a

unpackWits :: [WitStack] -> WitnessData
unpackWits = fmap (fmap getWitItem . getWitStack)

instance Arbitrary PeerIP where
  arbitrary = do
    b <- arbitrary
    if b
      then V4 <$> arbitrary
      else V6 <$> arbitrary

instance Arbitrary PeerAddr where
  arbitrary = PeerAddr <$> arbitrary <*> arbitrary
