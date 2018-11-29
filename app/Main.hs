module Main where

import Transform
import           Lib
import           Text.Megaparsec

main :: IO ()
main = do
  input <- getContents
  case parse expr "" input of
    Left  e -> putStr (errorBundlePretty e)
    Right x -> putStr . unlines . map show . logTransforms $ x
