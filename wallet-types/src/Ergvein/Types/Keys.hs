module Ergvein.Types.Keys (
    ScanKeyBox(..)
  , getLastUnusedKey
  , getPublicKeys
  , egvXPubCurrency
  , getExternalPubKeyIndex
  , extractXPubKeyFromEgv
  , getLabelFromEgvPubKey
  , unEgvXPrvKey
  , egvXPubKeyToEgvAddress
  , xPubToBtcAddr
  , xPubToErgAddr
  , extractAddrs
  -- * Reexport primary, non-versioned module
  , KeyPurpose(..)
  , EgvRootXPrvKey(..)
  , EgvXPrvKey(..)
  , EgvRootXPubKey(..)
  , EgvXPubKey(..)
  , xPubExport
  -- * Reexport latest version
  , EgvPubKeyBox(..)
  , PrvKeystore(..)
  , PubKeystore(..)
  -- * haskoin
  , XPrvKey(..)
  , XPubKey(..)
  ) where

import Data.Text              (Text)
import Data.Vector            (Vector)
import Ergvein.Crypto.Keys
import Ergvein.Types.Address
import Ergvein.Types.Currency
import Ergvein.Types.Keys.Box.Public (EgvPubKeyBox(..))
import Ergvein.Types.Keys.Prim
import Ergvein.Types.Keys.Store.Private (PrvKeystore(..))
import Ergvein.Types.Keys.Store.Public (PubKeystore(..))

import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.ByteString.Short as BSS
import qualified Data.Serialize        as SE

data ScanKeyBox = ScanKeyBox {
  scanBox'key     :: !EgvXPubKey
, scanBox'purpose :: !KeyPurpose
, scanBox'index   :: !Int
} deriving (Show)

-- ====================================================================
--      Utils
-- ====================================================================

unEgvXPrvKey :: EgvXPrvKey -> XPrvKey
unEgvXPrvKey key = case key of
  BtcXPrvKey k -> k
  ErgXPrvKey k -> k

egvXPubCurrency :: EgvXPubKey -> Currency
egvXPubCurrency val = case val of
  ErgXPubKey{} -> ERGO
  BtcXPubKey{} -> BTC

getLastUnusedKey :: KeyPurpose -> PubKeystore -> Maybe (Int, EgvPubKeyBox)
getLastUnusedKey kp PubKeystore{..} = go Nothing vector
  where
    vector = case kp of
      Internal -> pubKeystore'internal
      External -> pubKeystore'external
    go :: Maybe (Int, EgvPubKeyBox) -> Vector EgvPubKeyBox -> Maybe (Int, EgvPubKeyBox)
    go mk vec = if V.null vec then mk else let
      kb@(EgvPubKeyBox _ txs m) = V.last vec
      in if m || not (S.null txs)
        then mk
        else go (Just (V.length vec - 1, kb)) $ V.init vec

-- | Get all public keys in storage (external and internal) to scan for new transactions for them.
getPublicKeys :: PubKeystore -> Vector ScanKeyBox
getPublicKeys PubKeystore{..} = ext <> int
  where
    ext = V.imap (\i kb -> ScanKeyBox (pubKeyBox'key kb) External i) pubKeystore'external
    int = V.imap (\i kb -> ScanKeyBox (pubKeyBox'key kb) Internal i) pubKeystore'internal

getExternalPubKeyIndex :: PubKeystore -> Int
getExternalPubKeyIndex = V.length . pubKeystore'external

extractXPubKeyFromEgv :: EgvXPubKey -> XPubKey
extractXPubKeyFromEgv key = case key of
  ErgXPubKey k _ -> k
  BtcXPubKey k _ -> k

getLabelFromEgvPubKey :: EgvXPubKey -> Text
getLabelFromEgvPubKey key = case key of
  ErgXPubKey _ l -> l
  BtcXPubKey _ l -> l


xPubToBtcAddr :: XPubKey -> BtcAddress
xPubToBtcAddr key = pubKeyWitnessAddr $ wrapPubKey True (xPubKey key)

xPubToErgAddr :: XPubKey -> ErgAddress
xPubToErgAddr key = pubKeyErgAddr $ wrapPubKey True (xPubKey key)

pubKeyErgAddr :: PubKeyI -> ErgAddress
pubKeyErgAddr = ErgPubKeyAddress . VLAddr . BSS.toShort . SE.encode

egvXPubKeyToEgvAddress :: EgvXPubKey -> EgvAddress
egvXPubKeyToEgvAddress key = case key of
  ErgXPubKey k _ -> ErgAddress $ xPubToErgAddr k
  BtcXPubKey k _ -> BtcAddress $ xPubToBtcAddr k

-- | Extract addresses from keystore
extractAddrs :: PubKeystore -> Vector EgvAddress
extractAddrs pks = fmap (egvXPubKeyToEgvAddress . scanBox'key) $ getPublicKeys pks
