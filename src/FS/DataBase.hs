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
  (runDb, getOrderByID, myDefConnInfo,
   getForkByID, createFork, createFork',
   Fork(Fork)
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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    user_id T.CustomerUUID default=uuid_generate_v4()
    UniqueCustomerUUID user_id
    first_name String
    last_name String
    email T.Email
    country_code T.CountryCode
    phone T.PhoneNumber
    deriving Show
Order
    customer_id T.CustomerId
    product_id T.ProductId
    quantity T.Quantity
    order_status T.OrderStatus
    status_changed_at T.OrderStatusChangedDate
    deriving Show
Fork
    name T.ProductName
    merchant_id T.MerchantId
    price T.Price
    amount T.Quantity
    product_status T.ProductStatus
    deriving Show
|]

myDefConnInfo :: MySQL.ConnectInfo
myDefConnInfo = defaultConnectInfo {
  connectHost = "77.47.192.87",
  connectPort = 33321,
  connectUser = "ka7517",
  connectPassword = "123456",
  connectDatabase = "ka7517"
  }
             --     mkMySQLConnectInfo "77.47.192.87:33321" "ka7517" "123456" "ka7517"

-- inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
inBackend :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
inBackend action = runStdoutLoggingT $ withMySQLPool myDefConnInfo 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    action

getOrderByID :: Int -> IO (Maybe Order)
getOrderByID i = inBackend . get $ (toSqlKey (fromInteger.toInteger $ i) :: OrderId)

createOrder :: Order -> IO ()
createOrder (Order cid pid q _ _) = inBackend $ do
  now <- liftIO getCurrentTime
  orderId <- insert $ Order cid pid q  T.WaitingForPayment (T.getDay now)
  order <- get orderId
  liftIO $ print order

getForkByID :: Int -> IO (Maybe Fork)
getForkByID i = inBackend . get $ (toSqlKey (fromInteger.toInteger $ i) :: ForkId)

createFork :: Fork -> IO ()
createFork fork@(Fork name mid price amount status) = inBackend $ do
  now <- liftIO getCurrentTime
  orderId <- insert $ fork
  order <- get orderId
  liftIO $ print order

createFork' :: Fork -> IO ()
createFork' fork@(Fork name mid price amount status) = runDb myDefConnInfo $ do
  now <- liftIO getCurrentTime
  orderId <- insert $ fork
  order <- get orderId
  liftIO $ print order

runDb :: MySQL.ConnectInfo -> SqlPersistT (LoggingT IO) a -> IO a
runDb connInfo query = runStdoutLoggingT . withMySQLConn connInfo . runSqlConn $ query

-- getOrderByID :: MySQL.ConnectInfo -> T.OrderId -> IO [Entity Order]
-- getOrderByID connInfo oid = (runDb connInfo $ select)
--   where
--     select = rawSql "select ?? from order where order_id=?" [PersistInt64 .fromInteger.toInteger $ oid]

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
