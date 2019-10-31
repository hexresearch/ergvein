module Ergvein.Crypto.Keys(
    Base58
  , encodeBase58
  , decodeBase58
  , Mnemonic
  , toMnemonic
  , mnemonicToSeed
  , XPubKey
  , XPrvKey
  , xPubImport
  , xPrvImport
  , xPubExport
  , xPrvExport
  , getEntropy
  , makeXPrvKey
  , deriveXPubKey
  , xPubAddr
  , addrToString
  , xPubErgAddrString
  , example
  ) where

import Crypto.Hash
import Crypto.Hash.Algorithms
import Data.Text
import Ergvein.Crypto.Constants
import Ergvein.Crypto.WordLists
import Network.Haskoin.Address
import Network.Haskoin.Address.Base58
import Network.Haskoin.Constants
import Network.Haskoin.Keys
import Network.Haskoin.Util

import qualified Data.ByteArray                 as BA
import qualified Data.ByteString                as BS
import qualified System.Entropy                 as E

getEntropy :: IO Entropy
getEntropy = E.getEntropy defaultEntropyLength

-- | Convert BTC extended public key to a human-readable string.
xPubBtcAddrString :: Network -> XPubKey -> Text
xPubBtcAddrString net key = addrToString net addr
  where
    addr = xPubAddr key

-- | Convert ERGO extended public key to a human-readable string.
xPubErgAddrString :: Network -> XPubKey -> Text
xPubErgAddrString net key = encodeBase58 content
  where
    prefix = BS.singleton $ getAddrPrefix net
    keyByteString = exportPubKey True (xPubKey key)
    checkSumContent = BS.append prefix keyByteString
    checksum = BA.convert $ hashWith Blake2b_256 checkSumContent :: BS.ByteString
    content = BS.take 38 (BS.concat [prefix, keyByteString, checksum])

-- | Convert extended public key to a human-readable string.
xPubAddrToString :: Network -> XPubKey -> Either String Text
xPubAddrToString net key
  | net == btc || net == btcTest = Right $ xPubBtcAddrString net key
  | net == erg || net == ergTest = Right $ xPubErgAddrString net key
  | otherwise = Left "Unknown network type"

example :: IO ()
example = do
  ent <- getEntropy
  putStrLn "Entropy:"
  print ent
  let mnemonic = toMnemonic ent
  putStrLn "\nMnemonic:"
  print mnemonic
  let seed = mnemonic >>= mnemonicToSeed BS.empty
  putStrLn "\nSeed:"
  print seed
  let xPrvKey = fmap makeXPrvKey seed
  putStrLn "\nExtended private key:"
  print xPrvKey
  let xPubKey = fmap deriveXPubKey xPrvKey
  putStrLn "\nExtended public key:"
  print xPubKey
  let network = erg
  let address = fmap (xPubAddrToString network) xPubKey
  putStrLn "\nAddress:"
  print address
