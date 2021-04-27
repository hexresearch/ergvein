{-# LANGUAGE OverloadedLists #-}
module Ergvein.Wallet.Page.About(
    aboutPage
  ) where

import Sepulcas.Elements
import Ergvein.Wallet.Language
import Ergvein.Wallet.Localize
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
            -- text "Hexresearch team"
            -- elBR
            text "Anton Gushcha ncrashed@protonmail.com"
            elBR
            text "Dmitry Zuikov dzuikov@gmail.com"
            elBR
            text "Aleksandr Chekanin aminion@protonmail.com"
            elBR
            text "Vladimir Krutkin krutkinvs@gmail.com"
            elBR
            text "Levon Oganyan lemarwin42@protonmail.com"
            elBR
            text "Nardid Anatoly nazgull08@protonmail.com"
      divClass "about-distrib" $ localizedText AboutDistrib
    pure ()
  where
    aboutContent   = divClass "about-content"
    aboutRow       = divClass "about-content-row"
    aboutCellLabel = divClass "about-content-cell-label"
    aboutCellValue = divClass "about-content-cell-value"
    elBR           = el "br" blank
