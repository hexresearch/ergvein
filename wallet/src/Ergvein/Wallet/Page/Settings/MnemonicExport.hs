module Ergvein.Wallet.Page.Settings.MnemonicExport (
    mnemonicExportPage,
  ) where

import Control.Monad.IO.Class
import Ergvein.Crypto
import Ergvein.Text
import Data.Text.Encoding (encodeUtf8)
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

mnemonicExportPage :: MonadFront t m => Mnemonic -> m ()
mnemonicExportPage mnemonic = wrapperSimple True $ do
  divClass "password-setup-title" $ h4 $ localizedText PPSMnemonicTitle
  divClass "password-setup-descr" $ h5 $ localizedText PPSMnemonicDescr
  passE <- setupPassword
  nextWidget $
    ffor passE $ \pass ->
      Retractable
        { retractableNext = mnemonicExportResutlPage mnemonic pass,
          retractablePrev = Nothing
        }
  pure ()

mnemonicExportResutlPage :: MonadFront t m => Mnemonic -> Password -> m ()
mnemonicExportResutlPage mnemonic pass = do
  title <- localized STPSTitle
  let thisWidget = Just $ pure $ mnemonicExportResutlPage mnemonic pass
  wrapper True title thisWidget $ divClass "mnemonic-export-page" $ do
    encryptedMnemonic <- liftIO $ encryptMnemonic mnemonic pass
    h4 $ localizedText STPSMnemonicExportMsg
    base64D <- divClass "receive-qr" $ qrCodeWidgetWithData encryptedMnemonic
    parClass "mnemonic-export-text" $ text encryptedMnemonic
    pure ()

encryptMnemonic :: (MonadIO m, MonadRandom m) => Mnemonic -> Password -> m Text
encryptMnemonic mnemonic "" = pure mnemonic
encryptMnemonic mnemonic pass = do
  let mnemonicBS = encodeUtf8 mnemonic
  encryptedMnemonic <- encryptBSWithAEAD mnemonicBS pass
  case encryptedMnemonic of
    Left err -> fail $ T.unpack $ localizedShow English err
    Right encMnemonic -> do
      let encMnemonicBs = S.encode encMnemonic
          mnemonicText = encodeBase58CheckBtc encMnemonicBs
      pure mnemonicText
