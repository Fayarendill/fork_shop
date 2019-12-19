{-# LANGUAGE EmptyDataDecls       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
--import           Web.Scotty
--import qualified Web.Scotty                    as S

runDb :: MySQL.ConnectInfo -> SqlPersistM (ResourceT IO) a -> IO a
runDb connInfo query = runResourceT . withMySQLConn connInfo . runSqlConn $ query

readPosts ::  MySQL.ConnectInfo -> IO [Entity T.OrderInfo]
readPosts connInfo = (runDb connInfo $ selectList [] [LimitTo 10])

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
