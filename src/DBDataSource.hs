{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module DBDataSource
  ( -- PostId, PostContent
  -- , getPostIds
  -- , getPostContent
  -- , initDataSource
  -- , BlogRequest(..)
  -- , BlogDBException(..)
  ) where


import           Control.Exception
import           Control.Monad
import           Data.Hashable
import           Data.List
import qualified Data.Map             as Map
import           Data.Maybe
import           Data.Typeable
--import           Database.HaskellDB
import           Data.Functor.Classes
import           Database.HDBC.MySQL
import           Haxl.Core
import qualified Types                as T

-- | Request type

data DBRequest a where
  FetchCustomerInfo :: T.CustomerId -> DBRequest T.CustomerInfo
  FetchOrderInfo    :: T.OrderId -> DBRequest T.OrderInfo

deriving instance Show (DBRequest a)
deriving instance Typeable DBRequest

-- instance Show1 DBRequest where show1 = show

deriving instance Eq (DBRequest a)

instance Hashable (DBRequest a) where
  hashWithSalt salt (FetchCustomerInfo c) = hashWithSalt salt (0::Int, c)
  hashWithSalt salt (FetchOrderInfo o)    = hashWithSalt salt (1::Int, o)

-- |

-- | Requests

-- getCustomerInfo :: T.CustomerId -> GenHaxl u T.CustomerInfo
-- getCustomerInfo = dataFetc . FetchCustomerInfo

-- getOrderInfo :: T.OrderId -> GenHaxl u T.OrderInfo
-- getOrderInfo = dataFetc . FetchOrderInfo

-- |

-- | Data source implementation



-- |


