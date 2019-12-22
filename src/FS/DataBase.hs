{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE UndecidableInstances       #-}

module FS.DataBase
  (
  ) where

import           Control.Applicative
import           Control.Monad                 (forM_)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource  (ResourceT, runResourceT)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, getCurrentTime)
import qualified Database.MySQL.Simple         as MySQL
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.Sql
import           Database.Persist.TH
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              hiding (map)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Types                         as T

-- share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
-- User
--     name String
--     age Int
--     deriving Show
-- |]

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Customer
    cId T.CustomerId
    first_name String
    last_name String
    deriving Show
CustomerInfo
    ciId T.CustomerId
    email T.Email
    country_code T.CountryCode
    phone T.PhoneNumber
    deriving Show
OrderItems
    order_id T.OrderId
    product_id T.ProductId
    quantity T.Quantity
Order
    oId T.OrderId
    user_id T.CustomerId
    oStatus T.OrderStatus
    status_changed_at T.OrderStatusChangedDate
Product
    pId T.ProductId
    name T.ProductName
    merchant_id T.MerchantId
    price T.Price
    amount T.Quantity
    pStatus T.ProductStatus
|]

runDb :: MySQL.ConnectInfo -> SqlPersistT (ResourceT IO) a -> IO a
runDb connInfo query = runResourceT . withMySQLConn connInfo . runSqlConn $ query

getOrderInfoByID :: MySQL.ConnectInfo -> T.OrderId -> IO [Entity T.OrderInfo]
getOrderInfoByID connInfo = (runDb connInfo $ select)

--blaze = S.html . renderHtml

-- main = do
--   runDb myConnInfo $ runMigration migrateAll
--   scotty 3000 $ do
--     S.get "/create/:title" $ do
--       _title <- S.param "title"
--       now <- liftIO getCurrentTime
--       liftIO $ runDb $ insert $ Post _title "some content" now
--       S.redirect "/"

--     S.get "/" $ do
--       _posts <- liftIO readPosts connInfo
--       let posts = map (postTitle . entityVal) _posts
--       blaze $ do
--         ul $ do
--           forM_ posts $ \post -> li (toHtml post)
