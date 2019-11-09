module Types
  (
    CustomerId, OrderId, ProductId, MerchantId,
    Quantity, Price, CountryCode, PhoneNumber,
    Email, ProductName,
    ProductArrivedDate, OrderStatusChangedDate,
    ProductStatus, OrderStatus
  ) where

type CustomerId = Int
type OrderId    = Int
type ProductId  = Int
type MerchantId = Int

type Quantity    = Int
type Price       = Int --usd * 100
type CountryCode = Int
type PhoneNumber = Int

type Email       = String
type ProductName = String

type ProductArrivedDate     = String -- TO DO
type OrderStatusChangedDate = String -- TO DO

data ProductStatus =
  OutOfStock
  | InStock
  | RunningLow

data OrderStatus   =
  WaitingForPayment
  | PreparingForShipping
  | Shipping
  | Done
