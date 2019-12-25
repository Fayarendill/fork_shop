{-# LANGUAGE OverloadedStrings #-}

module Data.Error
  (
    Error(..),
    ParseError(..)
  ) where

class Error a where
  message :: a -> String

data ParseError
  = EnumParseError
  deriving (Show,Enum,Read,Eq)

instance Error ParseError where
 message EnumParseError = "Enum parse error"

-- instance Show ParseError where
--   show EnumParseError = "EnumParseError"

