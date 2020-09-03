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
  let deriveExternalPubKeysE = fforMaybe (updated pubStoreD) getMissingPubKeysCountHelper
  generateNewPubKeysByE BTC deriveExternalPubKeysE
  pure ()

getMissingPubKeysCountHelper :: PubStorage -> Maybe (Int, Int)
getMissingPubKeysCountHelper pubstorage = nothingIf (\(x, y) -> (x < 1) && (y < 1)) (external, internal)
  where
    external = getMissingPubKeysCount BTC External pubstorage
    internal = getMissingPubKeysCount BTC Internal pubstorage

generateNewPubKeysByE :: MonadFront t m => Currency -> Event t (Int, Int) -> m (Event t ())
generateNewPubKeysByE currency deriveE = do
  pubStoreD <- getPubStorageD
  let updatedKeyStorageE = attachPromptlyDynWith (derivePubKeys currency) pubStoreD deriveE
  modifyPubStorage "generateNewPubKeysByE" $ helper <$> updatedKeyStorageE
  where helper ks = Just . (pubStorageSetKeyStorage currency ks)
