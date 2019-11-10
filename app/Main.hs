module Main where

import           Data.Error
import           Enum.Print
import           Types

main :: IO ()
main = do
  let ps = readEnum "InStock" :: Either ParseError ProductStatus
  print ps
  print.showEnum $ ps
