module Main where

import Style

import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  TIO.writeFile "css/style.css" compileFrontendText
  putStrLn "done!"
