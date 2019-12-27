module Main where

import           Control.Monad.Logger
import           Data.Error
import           Enum.Print
import           FS.DataBase
import           FS.Web
import qualified Types                as T

main :: IO ()
main = forkShop
-- main = do
--   let fork  = (Fork "fork1" 0 22 10 T.RunningLow)
-- --  let order = (Fork "test" 0 8822 1 T.RunningLow)
--   let user  = (User T.nilCustomerUUIDStr "john" "Doe" "example@mail.com" 38 888)
--   createFork fork
--   deleteForkByID 14
--   --updateForkByID 15 fork


