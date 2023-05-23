{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeSchedules where

-- base
import Data.Kind
import Control.Concurrent
import Data.IORef
import Control.Monad.IO.Class

-- HList
import Data.HList

-- time-domain
import Data.TimeDomain

-- dunai
import Data.MonadicStreamFunction hiding (feedback)
import Data.MonadicStreamFunction.InternalCore hiding (feedback)

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
  EN :: Clock m cl => Event m cl -> EventNetwork m (HList '[cl])
  ENCons
    :: (Clock m cl0, Clock m cl, Clocks m (HList (cl ': cls)))
    => Event m cl0
    -> EventNetwork m (HList (cl ': cls))
    -> EventNetwork m (HList (cl0 ': cl ': cls))

data Tigris m cls = Tigris { clocks :: cls, en :: EventNetwork m cls }

infixr 6 .:.
(.:) :: (MonadIO m, Clock m cl0, Clock m cl, Clocks m (HList (cl ': cls)))
     => ClockedEvent m cl0
     -> Tigris m (HList (cl ': cls))
     -> Tigris m (HList (cl0 ': cl ': cls))
ClockedEvent {..} .: Tigris {..} = Tigris (HCons clock clocks) (ENCons event en)

infixr 6 .:
(.:.) :: (MonadIO m, Clock m cl1, Clock m cl2)
      => ClockedEvent m cl1
      -> ClockedEvent m cl2
      -> Tigris m (HList (cl1 ': '[cl2]))
clEv1 .:. (ClockedEvent clock2 event2) =
  clEv1 .: Tigris (HCons clock2 HNil) (EN event2)

data TimeInfo cl = TimeInfo
  { sinceLast :: Diff (Time cl)
  , sinceInit :: Diff (Time cl)
  , absolute :: Time cl
  , tag :: Tag cl
  }

type RunningClock m time tag = MSF m () (time, tag)

type RunningClockInit m time tag = m (RunningClock m time tag, time)

type RunningClocksInit m info = m (MSF m () info)

class TimeDomain (Time cl) => Clock m cl where
  initClock :: cl -> RunningClockInit m (Time cl) (Tag cl)
  type Time cl
  type Tag  cl

class Clocks m cls where
  initClocks :: (m () -> IO ()) -> cls -> RunningClocksInit m (ClocksInfo cls)
  _aux_      :: (m () -> IO ()) -> cls -> m (MVar (ClocksInfo cls), MSF m () ())
  type ClocksInfo cls

instance (MonadIO m, Clock m cl) => Clocks m (HList (cl ': '[])) where
  initClocks _ (HCons cl _) = do
    (runningClock, initTime) <- initClock cl
    return $ proc _ -> do
      (absolute, tag) <- runningClock  -< ()
      lastTime        <- iPre initTime -< absolute
      returnA                          -< TimeInfo 
        { sinceLast = absolute `diffTime` lastTime
        , sinceInit = absolute `diffTime` initTime
        , ..
        }
  _aux_ _ (HCons cl _) = do
    mvar <- liftIO newEmptyMVar
    (runningClock, initTime) <- initClock cl
    return
      ( mvar
      , proc _ -> do
          (absolute, tag) <- runningClock  -< ()
          lastTime        <- iPre initTime -< absolute
          arrM (liftIO . putMVar mvar) -< TimeInfo 
            { sinceLast = absolute `diffTime` lastTime
            , sinceInit = absolute `diffTime` initTime
            , ..
            }
      )

  type ClocksInfo (HList (cl ': '[])) = TimeInfo cl
  
instance (MonadIO m, Clock m cl1, Clocks m (HList (cl2 ': cls))) => Clocks m (HList (cl1 ': cl2 ': cls)) where
  initClocks unliftIO (HCons cl cls) = do
    mvar                     <- liftIO newEmptyMVar
    (clsMvar, runningClocks) <- _aux_ unliftIO cls
    -- forks to run first clock
    _ <- liftIO $ forkIO $ unliftIO $ do
      (runningClock, initTime) <- initClock cl
      reactimate $ proc _ -> do
        (absolute, tag) <- runningClock -< ()
        lastTime        <- iPre initTime -< absolute
        arrM (liftIO . putMVar mvar) -< Left $ TimeInfo 
          { sinceLast = absolute `diffTime` lastTime
          , sinceInit = absolute `diffTime` initTime
          , ..
          }
    -- forks to run remaining clocks
    _ <- liftIO $ forkIO $ unliftIO $ reactimate runningClocks
    -- forks to get infor from remaining clocks and put in main mvar
    _ <- liftIO $ forkIO $ unliftIO $ reactimate $
      constM (liftIO $ takeMVar clsMvar)
      >>> arr Right
      >>> arrM (liftIO . putMVar mvar)
    return $ constM $ liftIO $ takeMVar mvar

  _aux_ unliftIO (HCons cl cls) = do
    mvar                     <- liftIO newEmptyMVar
    (clsMvar, runningClocks) <- _aux_ unliftIO cls
    return
      ( mvar
      , constM $ do
          -- forks to run first clock
          _ <- liftIO $ forkIO $ unliftIO $ do
            (runningClock, initTime) <- initClock cl
            reactimate $ proc _ -> do
              (absolute, tag) <- runningClock -< ()
              lastTime        <- iPre initTime -< absolute
              arrM (liftIO . putMVar mvar) -< Left $ TimeInfo 
                { sinceLast = absolute `diffTime` lastTime
                , sinceInit = absolute `diffTime` initTime
                , ..
                }
          -- forks to run remaining clocks
          _ <- liftIO $ forkIO $ unliftIO $ reactimate runningClocks
          -- forks to get infor from remaining clocks and put in main mvar
          _ <- liftIO $ forkIO $ unliftIO $ reactimate $
            constM (liftIO $ takeMVar clsMvar)
            >>> arr Right
            >>> arrM (liftIO . putMVar mvar)
          return ()
      )

  type ClocksInfo (HList (cl1 ': cl2 ': cls))
    = Either (TimeInfo cl1) (ClocksInfo (HList (cl2 ': cls)))


eraseClocks
  :: (MonadIO m, Clocks m (HList (cl ': cls)))
  => Tigris m (HList (cl ': cls))
  -> (m () -> IO ())
  -> m (MSF m () ())
eraseClocks Tigris {..} unliftIO = do
  runningClocks <- initClocks unliftIO clocks
  return $ proc _ -> do
    clsInfo <- runningClocks -< ()
    eraseClocksEN en -< clsInfo

toMSF :: Monad m => Event m cl -> MSF m (TimeInfo cl) ()
toMSF event = MSF $ \clInfo -> do
  event' <- (unEvent event) clInfo
  return ((), toMSF event')

eraseClocksEN
  :: (Monad m, Clocks m (HList (cl ': cls)))
  => EventNetwork m (HList (cl ': cls))
  -> MSF m (ClocksInfo (HList (cl ': cls))) ()
eraseClocksEN (EN event) = proc clInfo -> do
  toMSF event -< clInfo
eraseClocksEN (ENCons event eventNetwork) = proc clsInfo -> do
  case clsInfo of
    Left clInfo    -> do
      toMSF event -< clInfo
    Right clsInfo' -> do
      eraseClocksEN eventNetwork -< clsInfo'

