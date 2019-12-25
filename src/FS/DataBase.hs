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
  (runDb, myDefConnInfo,
   getForkByID, updateForkByID, createFork, deleteForkByID,
   getOrderByID, createOrder,
   createUser, getUserByID,
   Fork(Fork), User(User), Order(Order)
  ) where

import           Control.Applicative
import           Control.Monad                 (forM_)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource  (ResourceT, runResourceT)
import qualified Data.GUID                     as G
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time                     (UTCTime, getCurrentTime)
import qualified Data.UUID                     as U
import qualified Database.MySQL.Simple         as MySQL
import           Database.Persist
import           Database.Persist.MySQL
import           Database.Persist.Sql
import           Database.Persist.TH
import           System.Random
import           Text.Blaze.Html.Renderer.Text
import           Text.Blaze.Html5              hiding (map)
import qualified Text.Blaze.Html5              as H
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5.Attributes   as A
import qualified Types                         as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    Id T.CustomerUUIDStr sql=user_id
    user_id T.CustomerUUIDStr default=uuid_generate_v4()
    UniqueCustomerUUIDStr user_id
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
  }             --     mkMySQLConnectInfo "77.47.192.87:33321" "ka7517" "123456" "ka7517"

runDb :: MySQL.ConnectInfo -> SqlPersistT (LoggingT IO) a -> IO a
runDb connInfo query = runStdoutLoggingT . withMySQLConn connInfo . runSqlConn $ query

-- inBackend :: ReaderT SqlBackend (NoLoggingT (ResourceT IO)) a-> IO a
-- inBackend :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
-- inBackend action = runStdoutLoggingT $ withMySQLPool myDefConnInfo 10 $ \pool -> liftIO $ do
--   flip runSqlPersistMPool pool $ do
--     runMigration migrateAll
--     action

-- | CRUD

getOrderByID :: Int -> IO (Maybe Order)
getOrderByID i = runDb myDefConnInfo . get $ (toSqlKey (fromInteger.toInteger $ i) :: OrderId)

updateOrderByID :: Int -> [Update Order] -> IO ()
updateOrderByID id updates = runDb myDefConnInfo $ update (toSqlKey (fromInteger.toInteger $ id) :: OrderId) updates

createOrder :: Order -> IO ()
createOrder (Order cid pid q _ _) = runDb myDefConnInfo $ do
  now <- liftIO getCurrentTime
  orderId <- insert $ Order cid pid q  T.WaitingForPayment (T.getDay now)
  order <- get orderId
  liftIO $ print order

deleteOrderByID :: Int -> IO ()
deleteOrderByID id = runDb myDefConnInfo . delete $ (toSqlKey (fromInteger.toInteger $ id) :: OrderId)


getForkByID :: Int -> IO (Maybe Fork)
getForkByID i = runDb myDefConnInfo . get $ (toSqlKey (fromInteger.toInteger $ i) :: ForkId)

updateForkByID :: Int -> [Update Fork] -> IO ()
updateForkByID id updates = runDb myDefConnInfo $ update (toSqlKey (fromInteger.toInteger $ id) :: ForkId) updates

createFork :: Fork -> IO ()
createFork fork@(Fork name mid price amount status) = runDb myDefConnInfo $ do
  now <- liftIO getCurrentTime
  orderId <- insert $ fork
  order <- get orderId
  liftIO $ print order

deleteForkByID :: Int -> IO ()
deleteForkByID id = runDb myDefConnInfo . delete $ (toSqlKey (fromInteger.toInteger $ id) :: ForkId)


getUserByID :: T.CustomerUUIDStr -> IO (Maybe (Entity User))
getUserByID uuid = runDb myDefConnInfo . getBy $ UniqueCustomerUUIDStr uuid

updateUserByID :: T.CustomerUUIDStr -> User -> IO ()
updateUserByID id fork@(User _ fname lname email ccode phone) = runDb myDefConnInfo $ userUpdate
  where
    userUpdate = updateWhere [UserUser_id ==. id] [UserFirst_name =. fname, UserLast_name =. lname, UserEmail =. email, UserCountry_code =. ccode, UserPhone =. phone]

createUser :: User -> IO ()
createUser (User _ fname lname email ccode phone) = runDb myDefConnInfo $ do
  uuidStr <- liftIO G.genString
  userId <- insert $ User uuidStr fname lname email ccode phone
  user <- getBy $ UniqueCustomerUUIDStr uuidStr
  liftIO $ print user

deleteUserByID :: T.CustomerUUIDStr -> IO ()
deleteUserByID uuid = runDb myDefConnInfo . deleteBy $ UniqueCustomerUUIDStr uuid

-- | use if RDBMS understands UUID type
-- createUser :: User -> IO ()
-- createUser (User _ fname lname email ccode phone) = runDb myDefConnInfo $ do
--   uuidTxt <- liftIO G.genText
--   userId <- insert $ User (T.CustomerUUID . getUuid . U.fromText $ uuidTxt) fname lname email ccode phone
--   user <- get userId
--   liftIO $ print user
--     where
--       getUuid (Just a) = a
--       getUuid Nothing  = U.nil

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
