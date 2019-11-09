{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module DataSource
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
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Typeable
--import           Database.HaskellDB
import           Database.HDBC.MySQL
import           Haxl.Core
import qualified Types               as T

