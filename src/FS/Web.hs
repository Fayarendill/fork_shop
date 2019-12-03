{-# LANGUAGE OverloadedStrings #-}

module FS.Web
  ( test
  ) where

import           Web.Scotty

test :: IO ()
test = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"
