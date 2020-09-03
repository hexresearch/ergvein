module Ergvein.Wallet.Worker.PubKeysGenerator
  (
    pubKeysGenerator
  , generateNewPubKeysByE
  ) where

import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Util

pubKeysGenerator :: MonadFront t m => m ()
pubKeysGenerator = do
  pubStoreD <- getPubStorageD
  let pubStoreUpdateE = updated pubStoreD
      deriveExternalPubKeysE = fforMaybe pubStoreUpdateE (nothingIf (< 1) . getMissingPubKeysCount BTC External)
      deriveInternalPubKeysE = fforMaybe pubStoreUpdateE (nothingIf (< 1) . getMissingPubKeysCount BTC Internal)
  generateNewPubKeysByE BTC $ (External,) <$> deriveExternalPubKeysE
  generateNewPubKeysByE BTC $ (Internal,) <$> deriveInternalPubKeysE
  pure ()

generateNewPubKeysByE :: MonadFront t m => Currency -> Event t (KeyPurpose, Int) -> m (Event t ())
generateNewPubKeysByE currency deriveE = do
  pubStoreD <- getPubStorageD
  let updatedKeyStorageE = attachPromptlyDynWith (derivePubKeys currency) pubStoreD deriveE
  modifyPubStorage "generateNewPubKeysByE" $ helper <$> updatedKeyStorageE
  where helper ks = Just . (pubStorageSetKeyStorage currency ks)
