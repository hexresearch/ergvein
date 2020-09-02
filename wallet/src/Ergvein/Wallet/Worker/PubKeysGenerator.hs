module Ergvein.Wallet.Worker.PubKeysGenerator
  (
    externalPubKeysGenerator
  , generateNewPubKeysByE
  ) where

import Ergvein.Types.Currency
import Ergvein.Types.Keys
import Ergvein.Types.Storage
import Ergvein.Wallet.Monad.Front
import Ergvein.Wallet.Monad.Storage
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Util

externalPubKeysGenerator :: MonadFront t m => m ()
externalPubKeysGenerator = do
  pubStoreD <- getPubStorageD
  let derivePubKeysE = fforMaybe (updated pubStoreD) (nothingIf (< 1) . getMissingPubKeysCount BTC External)
  generateNewPubKeysByE BTC $ (External,) <$> derivePubKeysE
  pure ()

generateNewPubKeysByE :: MonadFront t m => Currency -> Event t (KeyPurpose, Int) -> m (Event t ())
generateNewPubKeysByE currency deriveE = do
  pubStoreD <- getPubStorageD
  let updatedKeyStorageE = attachPromptlyDynWith (derivePubKeys currency) pubStoreD deriveE
  modifyPubStorage "generateNewPubKeysByE" $ helper <$> updatedKeyStorageE
  where helper ks = Just . (pubStorageSetKeyStorage currency ks)
