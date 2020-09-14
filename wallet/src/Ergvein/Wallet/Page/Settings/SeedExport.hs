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
import Ergvein.Wallet.Wrapper

import qualified Data.Serialize as S

seedExportPage :: MonadFront t m => m ()
seedExportPage = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure seedExportPage) $ do
    buildE <- getPostBuild
    seedE <- withWallet $
      ffor buildE $ \_ prvStorage -> do
        pure $ _prvStorage'seed prvStorage
    nextWidget $
      ffor seedE $ \seed ->
        Retractable
          { retractableNext = passwordPageExportSeed seed,
            retractablePrev = Nothing
          }
  pure ()

passwordPageExportSeed :: MonadFront t m => Seed -> m ()
passwordPageExportSeed seed = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSSeedTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSSeedDescr
  passE <- setupPassword
  nextWidget $
    ffor passE $ \pass ->
      Retractable
        { retractableNext = exportSeedPage2 seed pass,
          retractablePrev = Nothing
        }
  pure ()

exportSeedPage2 :: MonadFront t m => Seed -> Password -> m ()
exportSeedPage2 seed pass = do
  title <- localized STPSTitle
  wrapper True title (Just $ pure $ exportSeedPage2 seed pass) $ divClass "seed-export-page" $ do
    encryptedSeed <- liftIO $ encryptBS seed pass
    case encryptedSeed of
      Left err -> fail $ err
      Right encSeed ->  do
        let encSeedBs = S.encode encSeed
            seedText = encodeBase58CheckBtc encSeedBs
        h4 $ localizedText STPSSeedExportMsg
        base64D <- divClass "receive-qr" $ qrCodeWidgetWithData seedText BTC
        parClass "seed-export-seed-text" $ text seedText
        pure ()
