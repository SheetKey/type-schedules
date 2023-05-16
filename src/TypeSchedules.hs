module TypeSchedules where

import Data.HList

newtype Event m cl = Event { unEvent :: m (Event m cl) }
