module Data.Error
  (
    Error(..),
    ParseError(..)
  ) where

class Error a where
  lid :: a -> a

data ParseError
  = EnumParseError
  deriving (Show,Enum,Read,Eq)

instance Error ParseError where
  lid e = e

-- instance Show ParseError where
--   show EnumParseError = "EnumParseError"


