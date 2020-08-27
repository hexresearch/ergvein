module Ergvein.Aeson(
    A.deriveJSON
  , A.deriveToJSON
  , A.deriveFromJSON
  , ToJSON(..)
  , FromJSON(..)
  , ToJSONKey(..)
  , FromJSONKey(..)
  , aesonOptions
  , aesonOptionsStripPrefix
  , aesonOptionsStripToApostroph
  , defaultOptions
  , dropPrefixOptions
  , dropPunctuation
  , headToLower
  , statusAesonOptions
  , stripExactPrefix
  , stripToApostroph
  , untaggedSumOptions
  , untaggedToLowerSumOptions
  , unwrapUnaryOptions
  , encodeJson
  , decodeJson
  -- * IO
  , writeJson
  , writeJsonAtomic
  , readJson
  , readJsonLazy
  ) where

import Data.Aeson.TH (Options (..), SumEncoding (..), defaultOptions)
import Data.Aeson.Types (ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..))
import Data.Aeson(decode, decode', encode, eitherDecode')
import Data.Bifunctor (first)
import Data.Char (isUpper, toLower, isLower, isPunctuation)
import Data.List (findIndex)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import System.AtomicWrite.Writer.ByteString

import Ergvein.IO

import qualified Data.Aeson.TH as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import qualified Data.Text as T

-- | Converts first symbol to lower case
headToLower :: String -> String
headToLower [] = []
headToLower (x:xs@(y:_)) = (if (isLower y) then toLower else id) x : xs
                           -- So that HTTP remains as is
headToLower (x:xs) = toLower x : xs

-- | Drop prefix of name until first upper letter is occured
stripFieldPrefix :: String -> String
stripFieldPrefix str = case b of
  []   -> dropWhile (not . isUpper) str
  _:xs -> xs
  where
    (_, b) = L.span (/= '\'') str

-- | Strip prefix of name that exactly matches specified prefix
stripExactPrefix ::
    String -- ^ Prefix
 -> String -- ^ Name
 -> String -- ^ Name without prefix
stripExactPrefix = go
   where
   go [] name = name
   go (p : ps) name@(x : xs)
       | p == x = go ps xs
       | otherwise = name
   go _ [] = []

-- | Remove from names things like ' and etc
dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

-- | Drop upper case prefixes from constructor names
--
-- Example:
-- >>> stripConstructorPrefix "ABCombo"
-- "Combo"
--
-- >>> stripConstructorPrefix "Combo"
-- "Combo"
stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
   maybe t (flip drop t . decrementSafe) $ findIndex isLower t
 where
   decrementSafe 0 = 0
   decrementSafe i = i - 1

-- | Options for aeson TH generator, that generates following fields:
--
-- * without punctuation
-- * without lowercase prefix
--
-- And generates constructor tags without uppercase prefixes with
-- 'stripConstructorPrefix'.
--
-- Sums are encoded as one object with only one field corresponding the
-- constructor used.
dropPrefixOptions :: Options
dropPrefixOptions =
   A.defaultOptions
   { fieldLabelModifier = headToLower . stripFieldPrefix
   , constructorTagModifier = stripConstructorPrefix
   , omitNothingFields = True
   , sumEncoding = ObjectWithSingleField
   }

-- | Options for aeson TH generator, that generates following fields:
--
-- * without punctuation
-- * without lowercase prefix
--
-- And generates constructor tags without uppercase prefixes with
-- 'stripConstructorPrefix'.
--
-- Sums are encoded as one object with only one field corresponding the
-- constructor used.
aesonOptions :: Options
aesonOptions = aesonOptions' stripFieldPrefix

aesonOptionsStripPrefix :: String -> Options
aesonOptionsStripPrefix p = aesonOptions' (stripExactPrefix p)

stripToApostroph :: String -> String
stripToApostroph = reverse . takeWhile (/='\'') . reverse

aesonOptionsStripToApostroph :: Options
aesonOptionsStripToApostroph = A.defaultOptions
   { fieldLabelModifier = headToLower . stripToApostroph
   , constructorTagModifier = stripConstructorPrefix
   , omitNothingFields = True
   , sumEncoding = UntaggedValue
   }

aesonOptions' :: (String -> String) -> Options
aesonOptions' stripPrefix =
   A.defaultOptions
   { fieldLabelModifier = headToLower . stripPrefix . dropPunctuation
   , constructorTagModifier = stripConstructorPrefix
   , omitNothingFields = True
   , sumEncoding = UntaggedValue
   }

untaggedToLowerSumOptions :: Options
untaggedToLowerSumOptions = A.defaultOptions
   { sumEncoding = UntaggedValue
   , constructorTagModifier = fmap toLower
   }

untaggedSumOptions :: String -> Options
untaggedSumOptions = untaggedSumOptions' id

untaggedSumOptions' :: (String -> String) -> String -> Options
untaggedSumOptions' tagModifier prefix =
   A.defaultOptions
   { sumEncoding = UntaggedValue
   , constructorTagModifier = tagModifier . stripExactPrefix prefix
   }

unwrapUnaryOptions :: Options
unwrapUnaryOptions = A.defaultOptions { unwrapUnaryRecords = True }

statusAesonOptions :: String -> Options
statusAesonOptions prefix =
   A.defaultOptions
   { constructorTagModifier = headToLower
   , fieldLabelModifier  = headToLower . stripExactPrefix prefix
   , sumEncoding = ObjectWithSingleField
   }

--writeJson :: ToJSON a => FilePath -> a -> IO ()
--writeJson filename root = LB.writeFile filename $ encode root

writeJson :: ToJSON a => FilePath -> a -> IO ()
writeJson filename root = B.writeFile filename $ LB.toStrict $ encode root

writeJsonAtomic :: ToJSON a => FilePath -> a -> IO ()
writeJsonAtomic filename root = atomicWriteFile filename $ LB.toStrict $ encode root

readJsonLazy :: FromJSON a => FilePath -> IO (Maybe a)
readJsonLazy = fmap (decode =<<) . readLazyByteStringSafe

readJson :: FromJSON a => FilePath -> IO (Maybe a)
readJson = fmap (decode' . LB.fromStrict =<<) . readStrictByteStringSafe

encodeJson :: ToJSON a => a -> Text
encodeJson = decodeUtf8With lenientDecode . LB.toStrict . encode

decodeJson :: FromJSON a => Text -> Either Text a
decodeJson = first T.pack . eitherDecode' . LB.fromStrict . encodeUtf8
