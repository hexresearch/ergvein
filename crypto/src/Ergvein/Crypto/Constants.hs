module Ergvein.Crypto.Constants(
    Network(..)
  , NetworkTag(..)
  , defaultEntropyLength
  , btc
  , btcTest
  , erg
  , ergTest
  , getNetworkFromTag
  ) where

import Data.Aeson
import Data.String
import Data.Text(pack,unpack)
import Data.Version
import Network.Haskoin.Block
import Network.Haskoin.Constants
import Text.Read(readMaybe)

-- | Currently supported networks. Used for wrappers and whatnot
data NetworkTag = NetBTC | NetBTCTest | NetERG | NetERGTest
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Get network correspondent to a given tag
getNetworkFromTag :: NetworkTag -> Network
getNetworkFromTag t = case t of
  NetBTC     -> btc
  NetBTCTest -> btcTest
  NetERG     -> erg
  NetERGTest -> ergTest

instance ToJSON NetworkTag where
  toJSON = toJSON . show
instance FromJSON NetworkTag where
  parseJSON = withText "NetworkTag" $
    maybe (fail "Unknown network tag") pure . readMaybe . unpack

-- | According to the BIP32 the allowed size of entropy is between 16 and 64 bytes (32 bytes is advised).
-- The mnemonic must encode entropy in a multiple of 4 bytes.
-- With 32 bytes of entropy generated mnemonic will contain 24 words.
defaultEntropyLength :: Int
defaultEntropyLength = 32

version :: Version
version = Version [0,8,0] []

-- | Version of Haskoin Core package.
versionString :: IsString a => a
versionString = fromString (showVersion version)

-- | Ergo network. Symbol: ERG.
-- Warning: only fields marked as "+" are correct.
erg :: Network
erg =
    Network
    { getNetworkName = "erg"                                                            -- +
    , getNetworkIdent = "erg"                                                           -- +
    , getAddrPrefix = 1                                                                 -- +
    , getScriptPrefix = 5                                                               -- -
    , getSecretPrefix = 128                                                             -- -
    , getExtPubKeyPrefix = 0x0488b21e                                                   -- +
    , getExtSecretPrefix = 0x0488ade4                                                   -- +
    , getNetworkMagic = 0xf9beb4d9                                                      -- -
    , getGenesisHeader =                                                                -- -
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1231006505
              0x1d00ffff
              2083236893
            -- Hash 000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    , getMaxBlockSize = 1000000                                                         -- -
    , getMaxSatoshi = 2100000000000000                                                  -- -
    , getHaskoinUserAgent =                                                             -- -
          "/haskoin-btc:" <> versionString <> "/"
    , getDefaultPort = 8333                                                             -- -
    , getAllowMinDifficultyBlocks = False                                               -- -
    , getPowNoRetargetting = False                                                      -- -
    , getPowLimit =                                                                     -- -
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =                                                                   -- -
          ( 227931
          , "000000000000024b89b42a942fe0d9fea3bb44ab7bd1b19115dd6a759c0808b8")
    , getBip65Height = 388381                                                           -- -
    , getBip66Height = 363725                                                           -- -
    , getTargetTimespan = 14 * 24 * 60 * 60                                             -- -
    , getTargetSpacing = 10 * 60                                                        -- -
    , getCheckpoints = []                                                               -- -
    , getSeeds =[]                                                                      -- -
    , getBip44Coin = 429                                                                -- +
    , getSigHashForkId = Nothing                                                        -- -
    , getEdaBlockHeight = Nothing                                                       -- -
    , getDaaBlockHeight = Nothing                                                       -- -
    , getSegWit = True                                                                  -- -
    , getCashAddrPrefix = Nothing                                                       -- -
    , getBech32Prefix = Just "bc"                                                       -- -
    , getReplaceByFee = True                                                            -- -
    }

-- | Testnet for Ergo network.
-- Warning: only fields marked as "+" are correct.
ergTest :: Network
ergTest =
    Network
    { getNetworkName = "ergtest"                                                        -- +
    , getNetworkIdent = "ergTest"                                                       -- +
    , getAddrPrefix = 17                                                                -- +
    , getScriptPrefix = 196                                                             -- -
    , getSecretPrefix = 239                                                             -- -
    , getExtPubKeyPrefix = 0x043587cf                                                   -- +
    , getExtSecretPrefix = 0x04358394                                                   -- +
    , getNetworkMagic = 0x0b110907                                                      -- -
    , getGenesisHeader =                                                                -- -
          BlockHeader
              0x01
              "0000000000000000000000000000000000000000000000000000000000000000"
              "3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a"
              1296688602
              486604799
              414098458
            -- Hash 000000000933ea01ad0ee984209779baaec3ced90fa3f408719526f8d77f4943
    , getMaxBlockSize = 1000000                                                         -- -
    , getMaxSatoshi = 2100000000000000                                                  -- -
    , getHaskoinUserAgent = "/haskoin-btc-test:" <> versionString <> "/"                -- -
    , getDefaultPort = 18333                                                            -- -
    , getAllowMinDifficultyBlocks = True                                                -- -
    , getPowNoRetargetting = False                                                      -- -
    , getPowLimit =                                                                     -- -
          0x00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff
    , getBip34Block =                                                                   -- -
          ( 21111
          , "0000000023b3a96d3484e5abb3755c413e7d41500f8e2a5c3f0dd01299cd8ef8")
    , getBip65Height = 581885                                                           -- -
    , getBip66Height = 330776                                                           -- -
    , getTargetTimespan = 14 * 24 * 60 * 60                                             -- -
    , getTargetSpacing = 10 * 60                                                        -- -
    , getCheckpoints = []                                                               -- -
    , getSeeds = []                                                                     -- -
    , getBip44Coin = 1                                                                  -- -
    , getSigHashForkId = Nothing                                                        -- -
    , getEdaBlockHeight = Nothing                                                       -- -
    , getDaaBlockHeight = Nothing                                                       -- -
    , getSegWit = True                                                                  -- -
    , getCashAddrPrefix = Nothing                                                       -- -
    , getBech32Prefix = Just "tb"                                                       -- -
    , getReplaceByFee = True                                                            -- -
    }
