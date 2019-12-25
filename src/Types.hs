{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types
  (
    CustomerId, OrderId, ProductId, MerchantId,
    Quantity, Price, CountryCode, PhoneNumber,
    Email, ProductName,
    ProductArrivedDate, OrderStatusChangedDate,
    ProductStatus(OutOfStock, InStock, RunningLow),
    OrderStatus(WaitingForPayment, PreparingForShipping, Shipping, Done),
    OrderInfo, CustomerInfo,
    CustomerUUID, getDay
  ) where

import           Control.Error
import           Control.Lens
import           Data.ByteString      (ByteString)
import           Data.Error
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.UUID
import           Enum.Print
import           System.Random
-- import           Chronos.Types        (Datetime, Day)
import           Database.Persist.Sql

data CustomerUUID = CustomerUUID
  {
    _customerUuid :: UUID
  } deriving (Show, Eq, Ord, Random)

makeLenses ''CustomerUUID

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
  toPersistValue OutOfStock = PersistInt64 1
  toPersistValue InStock    = PersistInt64 2
  toPersistValue RunningLow = PersistInt64 3

  fromPersistValue s = result. readEnum.clean $ s
    where
      clean            = T.unpack. T.filter (\x -> x/='\\' && x/='\"') . T.replace "PersistText " "" . T.pack . show
      result (Left a)  = Left . T.pack . (++) (message a ) $ (show $ s)
      result (Right b) = Right b


instance PersistField OrderStatus where
  toPersistValue WaitingForPayment    = PersistInt64 1
  toPersistValue PreparingForShipping = PersistInt64 2
  toPersistValue Shipping             = PersistInt64 3
  toPersistValue Done                 = PersistInt64 4

  fromPersistValue s = result. readEnum.clean $ s
    where
      clean            = T.unpack. T.filter (\x -> x/='\\' && x/='\"') . T.replace "PersistText " "" . T.pack . show
      result (Left a)  = Left . T.pack . (++) (message a ) $ (show $ s)
      result (Right b) = Right b

instance PersistFieldSql OrderStatus where
  sqlType _ = SqlOther "order_status"

instance PersistFieldSql ProductStatus where
  sqlType _ = SqlOther "product_status"


instance PersistFieldSql CustomerUUID where
  sqlType = const $ SqlOther "uuid"

instance PersistField CustomerUUID where
  toPersistValue = toPersistValueUUID customerUuid
  fromPersistValue = fromPersistValueUUID customerUuid

_ASCIIBytes :: Prism' ByteString UUID
_ASCIIBytes = prism toASCIIBytes $ \bs -> note bs $ fromASCIIBytes bs

toPersistValueUUID :: Iso' a UUID -> a -> PersistValue
toPersistValueUUID i a = PersistDbSpecific $ a ^. i . re _ASCIIBytes

fromPersistValueUUID :: Iso' a UUID -> PersistValue -> Either Text a
fromPersistValueUUID i (PersistDbSpecific bs) =
  note "Could not parse UUID" $ bs ^? _ASCIIBytes . from i
fromPersistValueUUID _ x = Left $ "Invalid value for UUID: " <> showT x

showT :: Show a => a -> Text
showT = T.pack . show

getDay :: UTCTime -> Day
getDay (UTCTime d _) = d

-- instance 'PersistField' OrderStatus where
--   'toPersistValue' s = case s of
--     OnWaitingForPayment -> 'PersistInt64' 0
--     PreparingForShipping -> 'PersistInt64' 1
--     Shipping -> 'PersistInt64' 2
--     Done -> 'PersistInt64' 3
--   'fromPersistValue' ('PersistInt64' b) = if b then 'Right' On else 'Right' Off
--   'fromPersistValue' x = Left $ "File.hs: When trying to deserialize a Switch: expected PersistBool, received: " <> T.pack (show x)
