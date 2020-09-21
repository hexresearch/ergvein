module Ergvein.Wallet.Page.Settings.SeedExport (
    seedExportPage,
  ) where

import Control.Monad.IO.Class
import Ergvein.Crypto
import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Types.Storage
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.Password
import Ergvein.Wallet.Localization.Settings
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Password
import Ergvein.Wallet.Storage
import Ergvein.Wallet.Storage.Util
import Ergvein.Wallet.Wrapper

import qualified Data.Serialize as S
import qualified Data.Text as T

seedExportPage :: MonadFront t m => Seed -> m ()
seedExportPage seed = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSSeedTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSSeedDescr
  passE <- setupPassword
  nextWidget $
    ffor passE $ \pass ->
      Retractable
        { retractableNext = seedExportResutlPage seed pass,
          retractablePrev = Nothing
        }
  pure ()

seedExportResutlPage :: MonadFront t m => Seed -> Password -> m ()
seedExportResutlPage seed pass = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure $ seedExportResutlPage seed pass) $ divClass "seed-export-page" $ do
    encryptedSeed <- liftIO $ encryptBSWithAEAD seed pass
    case encryptedSeed of
      Left err -> fail $ T.unpack $ localizedShow English err
      Right encSeed ->  do
        let encSeedBs = S.encode encSeed
            seedText = encodeBase58CheckBtc encSeedBs
        h4 $ localizedText STPSSeedExportMsg
        base64D <- divClass "receive-qr" $ qrCodeWidgetWithData seedText
        parClass "seed-export-seed-text" $ text seedText
        pure ()
