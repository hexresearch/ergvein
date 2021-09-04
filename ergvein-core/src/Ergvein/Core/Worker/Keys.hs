module Ergvein.Core.Worker.Keys
  (
    pubKeysGeneratorBtc
  , generateNewPubKeysByE
  ) where

import Data.Functor
import Reflex.Flunky
import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Core.Wallet
import Ergvein.Core.Store

pubKeysGeneratorBtc :: MonadWallet t m => m ()
pubKeysGeneratorBtc = do
  pubStoreD <- getPubStorageD
  let deriveExternalPubKeysE = fforMaybe (updated pubStoreD) getMissingPubKeysCountHelper
  void $ generateNewPubKeysByE BTC deriveExternalPubKeysE

getMissingPubKeysCountHelper :: PubStorage -> Maybe (Int, Int)
getMissingPubKeysCountHelper pubstorage = nothingIf (\(x, y) -> (x < 1) && (y < 1)) (external, internal)
  where
    external = getMissingPubKeysCount BTC External pubstorage
    internal = getMissingPubKeysCount BTC Internal pubstorage

generateNewPubKeysByE :: MonadWallet t m => Currency -> Event t (Int, Int) -> m (Event t ())
generateNewPubKeysByE currency deriveE = do
  pubStoreD <- getPubStorageD
  let updatedKeyStorageE = attachPromptlyDynWith (derivePubKeys currency) pubStoreD deriveE
  modifyPubStorage "generateNewPubKeysByE" $ helper <$> updatedKeyStorageE
  where helper ks = Just . (pubStorageSetKeyStorage currency ks)
