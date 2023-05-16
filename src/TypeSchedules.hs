{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeSchedules where

-- base
import Data.Kind

-- HList
import Data.HList

-- time-domain
import Data.TimeDomain

-- dunai
import Data.MonadicStreamFunction hiding (feedback)

newtype Event m cl = Event { unEvent :: TimeInfo cl -> m (Event m cl) }

ev :: Monad m => m () -> Event m cl
ev e = go
  where
    go = Event $ \_ -> do
      e
      return go

feedback :: Monad m => a -> (a -> m a) -> Event m cl
feedback a f = Event $ \_ -> do
  a' <- f a
  return (feedback a' f)

data ClockedEvent m cl = ClockedEvent { clock :: cl, event :: Event m cl }

-- dont expose: add to an internal module
data EventNetwork m cls where
  EN :: Event m cl -> EventNetwork m (HList '[cl])
  ENCons :: Event m cl -> EventNetwork m (HList cls) -> EventNetwork m (HList (cl ': cls))

data Tigris m cls = Tigris { clocks :: cls, en :: EventNetwork m cls }

infixr 6 .:.
(.:.) :: ClockedEvent m cl -> Tigris m (HList (cls :: [Type])) -> Tigris m (HList (cl ': cls))
ClockedEvent {..} .:. Tigris {..} = Tigris (HCons clock clocks) (ENCons event en)

infixr 6 .:
(.:) :: ClockedEvent m cl1 -> ClockedEvent m cl2 -> Tigris m (HList (cl1 ': '[cl2]))
(ClockedEvent clock1 event1) .: (ClockedEvent clock2 event2) =
  Tigris (HCons clock1 (HCons clock2 HNil)) (ENCons event1 (EN event2))

data TimeInfo cl = TimeInfo
  { sinceLast :: Diff (Time cl)
  , sinceInit :: Diff (Time cl)
  , absolute :: Time cl
  , tag :: Tag cl
  }

type RunningClock m time tag = MSF m () (time, tag)

type RunningClockInit m time tag = m (RunningClock m time tag, time)

type RunningClocksInit m info = MSF m () info

class Clock m cl where
  initClock :: cl -> RunningClockInit m (Time cl) (Tag cl)
  type Time cl
  type Tag  cl

class Clocks m cls where
  initClocks :: cls -> RunningClocksInit m (ClocksInfo cls)
  type ClocksInfo cls

instance Clocks m (HList '[]) where
  initClocks = undefined
  type ClocksInfo (HList '[]) = ()
  
instance (Clock m cl, Clocks m (HList cls)) => Clocks m (HList (cl ': cls)) where
  initClocks = undefined
  type ClocksInfo (HList (cl ': cls)) = Either (TimeInfo cl) (ClocksInfo (HList cls))

eraseClocks
  :: (Monad m, Clocks m cls)
  => Tigris m cls
  -> m ()
eraseClocks Tigris {..} = do
  undefined

--eraseClocksEN :: (Monad m, Clocks cls) => EventNetwork m cls -> MSF m (ClocksInfo
