{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}

module Lib
    ( someFunc
    ) where

import           Database.MySQL.Base
import           DataSource
import           Haxl.Prelude
someFunc :: IO ()
someFunc = putStrLn "someFunc"
