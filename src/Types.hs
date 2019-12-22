{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types
  (
    CustomerId, OrderId, ProductId, MerchantId,
    Quantity, Price, CountryCode, PhoneNumber,
    Email, ProductName,
    ProductArrivedDate, OrderStatusChangedDate,
    ProductStatus, OrderStatus,
    OrderInfo, CustomerInfo
  ) where

import           Data.Time.Calendar
-- import           Chronos.Types        (Datetime, Day)
import           Database.Persist.Sql

type CustomerId = Int
type OrderId    = Int
type ProductId  = Int
type MerchantId = Int

type Quantity    = Int
type Price       = Int -- usd * 100
type CountryCode = Int
type PhoneNumber = Int

type Email       = String
type ProductName = String

type ProductArrivedDate     = Day
type OrderStatusChangedDate = Day

data ProductStatus
  = OutOfStock
  | InStock
  | RunningLow
  deriving (Show,Enum,Read,Eq)

data OrderStatus
  = WaitingForPayment
  | PreparingForShipping
  | Shipping
  | Done
  deriving (Show,Enum,Read,Eq)

data OrderInfo = OrderInfo
  { oId              :: OrderId,
    oUserId          :: CustomerId,
    oStatus          :: OrderStatus,
    oStatusChangedAt :: OrderStatusChangedDate
  }

data CustomerInfo = CustomerInfo
  { cId          :: CustomerId,
    cEmail       :: Email,
    cCountryCode :: CountryCode,
    cPhone       :: PhoneNumber
  }

instance PersistField ProductStatus where
  toPersistValue OutOfStock = PersistInt64 0
  toPersistValue InStock    = PersistInt64 1
  toPersistValue RunningLow = PersistInt64 2

  fromPersistValue (PersistInt64 0) = Right OutOfStock
  fromPersistValue (PersistInt64 1) = Right InStock
  fromPersistValue (PersistInt64 2) = Right RunningLow
  fromPersistValue _                = Left "Enum parse error"

instance PersistField OrderStatus where
  toPersistValue WaitingForPayment    = PersistInt64 0
  toPersistValue PreparingForShipping = PersistInt64 1
  toPersistValue Shipping             = PersistInt64 2
  toPersistValue Done                 = PersistInt64 3

  fromPersistValue (PersistInt64 0) = Right WaitingForPayment
  fromPersistValue (PersistInt64 1) = Right PreparingForShipping
  fromPersistValue (PersistInt64 2) = Right Shipping
  fromPersistValue (PersistInt64 3) = Right Done
  fromPersistValue _                = Left "Enum parse error"

instance PersistFieldSql OrderStatus where
  sqlType _ = SqlOther "order_status"

instance PersistFieldSql ProductStatus where
  sqlType _ = SqlOther "product_status"

-- instance 'PersistField' OrderStatus where
--   'toPersistValue' s = case s of
--     OnWaitingForPayment -> 'PersistInt64' 0
--     PreparingForShipping -> 'PersistInt64' 1
--     Shipping -> 'PersistInt64' 2
--     Done -> 'PersistInt64' 3
--   'fromPersistValue' ('PersistInt64' b) = if b then 'Right' On else 'Right' Off
--   'fromPersistValue' x = Left $ "File.hs: When trying to deserialize a Switch: expected PersistBool, received: " <> T.pack (show x)
