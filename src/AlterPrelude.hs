{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module AlterPrelude
  (
    module Haxl.Prelude,
    lookup
  ) where
import           Data.Either
import           Data.Error
import           Haxl.Prelude hiding (lookup)
import           Prelude      ()

lookup                  :: (Eq a, Error e) => e -> a -> [(a,b)] -> Either e b
lookup err _ []         = Left err
lookup err key ((x,y):xys)
    | key == x          = Right y
    | otherwise         = lookup err key xys
