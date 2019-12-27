{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module FS.Web
  ( forkShop
  ) where

-- import           Web.Scotty
-- import           Text.Hamlet
import qualified Text.Cassius as SC
import qualified Text.Lucius  as SL
import           Yesod

data ForkShop = ForkShop

mkYesod "ForkShop" [parseRoutes|
/ HomeR GET
/market MarketR GET
/orders OrdersR GET
|]

instance Yesod ForkShop

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  -- addStylesheet ($(SC.cassiusFile "cassius/home-menu.cassius") ())
  -- addStylesheet $ StaticR css_bootstrap_css
  -- toWidget $(SC.cassiusFile "templates/home-menu.cassius")
  toWidget $(SL.luciusFileReload "templates/home-menu.lucius")
  $(whamletFile "templates/home-menu.hamlet")
  where
    isLoggedIn = True
-- getHomeR = defaultLayout [whamlet|
-- This is home!
--     <a href=@{MarketR}>Go to market!
--     <a href=@{OrdersR}>Go to orders!|]

getMarketR :: Handler Html
getMarketR = defaultLayout [whamlet| This market!
<a href=@{HomeR}>Go to home!|]

getOrdersR :: Handler Html
getOrdersR = defaultLayout [whamlet| This orders!
<a href=@{HomeR}>Go to home!|]



forkShop :: IO ()
forkShop = warp 3000 ForkShop

-- test :: IO ()
-- test = scotty 3000 $ do
--   get "/" $ do
--     html "Hello World!"
