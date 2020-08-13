{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.About(
    aboutPage
  ) where

import Ergvein.Text
import Ergvein.Types.Currency
import Ergvein.Wallet.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localization.About
import Ergvein.Wallet.Menu
import Ergvein.Wallet.Monad
import Ergvein.Wallet.Version
import Ergvein.Wallet.Wrapper

aboutPage :: MonadFront t m => m ()
aboutPage = do
  title <- localized AboutTitle
  wrapper False title (Just $ pure aboutPage) $ do
    h3 $ localizedText $ AboutTitle
    elAttr "hr" [("class","about-hr-sep")] blank
    divClass "about-wrapper" $ do
      aboutContent $ do
        aboutRow $ do
          aboutCellLabel $ localizedText AboutVersion
          aboutCellValue $ text $ makeVersionString version
        aboutRow $ do
          aboutCellLabel $ localizedText AboutLicence
          aboutCellValue $ text "MIT License"
        aboutRow $ do
          aboutCellLabel $ localizedText AboutHomepage
          let homepageUrl = "https://cypra.io"
          aboutCellValue $ hyperlink "link" homepageUrl homepageUrl
        aboutRow $ do
          aboutCellLabel $ localizedText AboutDevelopers
          aboutCellValue $ do
            text "Hexresearch team"
            elBR
            text "Anton Gushcha ncrashed@protonmail.com"
            elBR
            text "Dmitry Zuikov dzuikov@gmail.com"
      divClass "about-distrib" $ localizedText AboutDistrib
    pure ()
  where
    aboutContent   = divClass "about-content"
    aboutRow       = divClass "about-content-row"
    aboutCellLabel = divClass "about-content-cell-label"
    aboutCellValue = divClass "about-content-cell-value"
    elBR           = el "br" blank
