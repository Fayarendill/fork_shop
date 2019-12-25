{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RebindableSyntax   #-}

module Enum.Print
  (
    showEnum,
    readEnum
  ) where

import           AlterPrelude
import           Control.Arrow
import           Data.Char     (toLower, toUpper)
import           Data.Either
import           Data.Error
import           Data.Typeable
-- import           Haxl.Prelude  hiding (lookup)
import           Prelude       hiding (lookup)
import           Text.Regex


-- showEnum :: (Show a) => a -> String
-- showEnum = flatten . show where
--   flatten     = map toLower . upperToDash
--   upperToDash = flip (subRegex (mkRegex "([a-z])([A-Z])")) "\\1_\\2"
--               . flip (subRegex (mkRegex "_")) "."

-- | Print an Enum for external use.
showEnum :: (Show a) => a -> String
showEnum = toSnake . show

-- | Parse the Enum
readEnum   :: (Enum a, Show a, Read a) => String -> Either ParseError a
readEnum s = (lookup EnumParseError s (map (toSnake.show &&& id) [toEnum 0..]))

toCamel :: String -> String
toCamel = (transformFst toUpper) . toMixed

toMixed :: String -> String
toMixed = subRegex' (mkRegex "_[a-zA-Z]+") go
  where
    subRegex' :: Regex -> (String -> String) -> String -> String
    subRegex' r n s = case matchRegexAll r s of
      Nothing           -> s
      Just (b, m, a, _) -> b ++ n m ++ (subRegex' r n a)

    go :: String -> String
    go []     = []
    go (x:xs) = transformFst toUpper xs

transformFst :: (Char -> Char) -> String -> String
transformFst _   ""   = ""
transformFst f (x:xs) = f x:xs

toSnake :: String -> String
toSnake = downcase . go
  where
    downcase = map toLower
    go s = case subRegex (mkRegex "[A-Z]") s "_\\0" of
      ('_':xs) -> xs
      x        -> x
