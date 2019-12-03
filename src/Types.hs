{-# LANGUAGE RecordWildCards #-}
module Types
  (
    CustomerId, OrderId, ProductId, MerchantId,
    Quantity, Price, CountryCode, PhoneNumber,
    Email, ProductName,
    ProductArrivedDate, OrderStatusChangedDate,
    ProductStatus, OrderStatus,
    OrderInfo, CustomerInfo
  ) where

import           Chronos.Types (Datetime)

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

type ProductArrivedDate     = Datetime
type OrderStatusChangedDate = Datetime

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
