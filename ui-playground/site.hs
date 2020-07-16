--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx =
                    constField "title" "Home" `mappend`
                    constField "homepage" "" `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
    match "templates/*" $ compile templateCompiler
