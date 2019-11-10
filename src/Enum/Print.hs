{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RebindableSyntax   #-}

module Enum.Print
  (
  ) where

import           AlterPrelude
import           Control.Arrow
import           Data.Char     (toLower)
import           Data.Either
import           Data.Error
import           Data.Typeable
import           Haxl.Prelude  hiding (lookup)
import           Prelude       ()
import           Text.Regex

-- | Print an Enum for external use.
showEnum :: (Show a) => a -> String
showEnum = flatten . show where
  flatten = map toLower . upperToDash
  upperToDash = flip (subRegex (mkRegex "([a-z])([A-Z])")) "\\1_\\2"
              . flip (subRegex (mkRegex "_")) "."

-- | Parse the Enum
readEnum :: (Enum a, Show a, Read a) => String -> Either ParseError a
readEnum = \x -> (lookup (EnumParseError :: ParseError) x (map (showEnum &&& id) [toEnum 0 ..]))
