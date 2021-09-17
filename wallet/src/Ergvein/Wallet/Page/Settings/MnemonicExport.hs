{-# LANGUAGE CPP #-}

module Ergvein.Wallet.Page.Settings.MnemonicExport (
    mnemonicExportPage
  , mnemonicExportResutlPage
  ) where

import Control.Monad.IO.Class
import Data.Text.Encoding (encodeUtf8)

import Ergvein.Crypto
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Page.QRCode
import Ergvein.Wallet.Password
import Ergvein.Wallet.Wrapper
import Sepulcas.Clipboard
import Sepulcas.Elements
import Sepulcas.Share

import qualified Data.Serialize as S
import qualified Data.Text as T

mnemonicExportPage :: MonadFront t m => m ()
mnemonicExportPage = do
  title <- localized STPSButMnemonicExport
  let thisWidget = Just $ pure mnemonicExportPage
  wrapper True title thisWidget $ do
    divClass "password-setup-title" $ h4 $ localizedText PPSMnemonicTitle
    divClass "password-setup-descr" $ h5 $ localizedText PPSMnemonicDescr
    passE <- setupPassword =<< submitSetBtn
    mnemonicPassE <- withWallet $ ffor passE $ \pass prvStorage -> pure (_prvStorage'mnemonic prvStorage, pass)
    void $ nextWidget $ ffor mnemonicPassE $ \(mnemonic, pass) ->
      Retractable
        { retractableNext = mnemonicExportResutlPage mnemonic pass,
          retractablePrev = thisWidget
        }

mnemonicExportResutlPage :: MonadFront t m => Mnemonic -> Password -> m ()
mnemonicExportResutlPage mnemonic pass = do
  title <- localized STPSTitle
  buildE <- getPostBuild
  encryptedMnemonic <- liftIO $ encryptMnemonic mnemonic pass
  void $ networkHold (pure ()) $ ffor buildE $ const $
    -- We don't want to keep this page on the retract stack
    wrapper True title Nothing $ divClass "mnemonic-export-page" $ do
      h4 $ localizedText STPSMnemonicExportMsg
      base64D <- divClass "mb-2" $ qrCodeWidgetWithData encryptedMnemonic
      let mnemonicClass = if T.null pass then "" else "word-break-all"
      parClass mnemonicClass $ text encryptedMnemonic
      void $ divClass "mnemonic-export-buttons-wrapper" $ do
        copyE <- copyBtn
        void $ clipboardCopy (encryptedMnemonic <$ copyE)
        when isAndroid $ do
          void $ shareShareUrl . (encryptedMnemonic <$) =<< shareBtn
          shareQRE <- shareQRBtn
          void $ shareShareQR $ attachPromptlyDynWithMaybe (\m _ -> (, "qr_code") <$> m) base64D shareQRE

copyBtn :: MonadFront t m => m (Event t ())
copyBtn = divClass "mnemonic-export-btn-wrapper" $ outlineTextIconButton CSCopy "fas fa-copy fa-lg"

shareBtn :: MonadFront t m => m (Event t ())
shareBtn = divClass "mnemonic-export-btn-wrapper" $ outlineTextIconButton CSShare "fas fa-share-alt fa-lg"

shareQRBtn :: MonadFront t m => m (Event t ())
shareQRBtn = divClass "mnemonic-export-btn-wrapper" $ outlineTextIconButton CSShareQR "fas fa-qrcode fa-lg"

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
