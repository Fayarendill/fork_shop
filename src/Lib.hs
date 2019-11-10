{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Lib
    ( someFunc
    ) where

import           DBDataSource
import           Haxl.Prelude
someFunc :: IO ()
someFunc = putStrLn "someFunc"
