{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | All the building blocks to allow rules to build events.
module Imprevu.Events
--   onEvent, onEvent_, onEventOnce,
--   delEvent,
--   getEvents, getEvent,
--   getIntermediateResults,
--   schedule, schedule_, schedule', schedule'_,
--   getCurrentTime,
--   oneWeek, oneDay, oneHour, oneMinute,
--   timeEvent, messageEvent, victoryEvent, playerEvent, ruleEvent,
--   signalEvent, inputFormSignal,
--   liftEvent
    where

import Imprevu.Types
import Imprevu.SysMgt
import Control.Monad.Except
import Data.Typeable
import Data.Time hiding (getCurrentTime)
import Data.Time.Recurrence hiding (filter)
import Data.List
import Data.Maybe
import Safe

type Msg m = Signal String m

class (Typeable n, Applicative n, Monad n) => EvMgt n where
   --Events management
   onEvent         :: (Typeable a, Show a) => EventM n a -> ((EventNumber, a) -> n ()) -> n EventNumber
   sendMessage     :: (Typeable a, Show a, Eq a) => Msg a -> a -> n ()
   getEventResult  :: (Typeable a, Show a) => EventNumber -> EventM n a -> n (Maybe a)
   isEventActive   :: EventNumber -> n Bool 
   delEvent        :: EventNumber -> n Bool

partial :: (MonadError String n) => String -> n (Maybe a) -> n a
partial s nm = do
   m <- nm
   case m of
      Just a -> return a
      Nothing -> throwError s

-- * Events

-- | register a callback on an event, disregard the event number
onEvent_ :: (Typeable a, Show a, EvMgt n) => EventM n a -> (a -> n ()) -> n EventNumber
onEvent_ e h = onEvent e (\(_, d) -> h d)

-- | set an handler for an event that will be triggered only once
onEventOnce :: (Typeable a, Show a, EvMgt n) => EventM n a -> (a -> n ()) -> n EventNumber
onEventOnce e h = do
    let handler (en, ed) = delEvent en >> h ed
    onEvent e handler

-- | on the provided schedule, the supplied function will be called
schedule :: (EvMgt n, SysMgt n) => Schedule Freq -> (UTCTime -> n ()) -> n ()
schedule sched f = do
    now <- getCurrentTime
    let next = head $ starting now sched
    if next == now then executeAndScheduleNext f sched now
                   else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext f sched

executeAndScheduleNext :: (EvMgt n) => (UTCTime -> n ()) -> Schedule Freq -> UTCTime -> n ()
executeAndScheduleNext f sched now = do
   f now
   let rest = drop 1 $ starting now sched
   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext f sched

schedule_ :: (EvMgt n, SysMgt n) => Schedule Freq -> n () -> n ()
schedule_ ts f = schedule ts (const f)

--at each time provided, the supplied function will be called
schedule' :: (EvMgt n, SysMgt n) => [UTCTime] -> (UTCTime -> n ()) -> n ()
schedule' sched f = do
   let sched' = sort sched
   now <- getCurrentTime
   let nextMay = headMay $ filter (>=now) sched'
   case nextMay of
       Just next -> if next == now then executeAndScheduleNext' f sched' now
                                  else void $ onEventOnce (timeEvent next) $ executeAndScheduleNext' f sched'
       Nothing -> return ()

executeAndScheduleNext' :: (EvMgt n) => (UTCTime -> n ()) -> [UTCTime] -> UTCTime -> n ()
executeAndScheduleNext' f sched now = do
   f now
   let rest = drop 1 sched
   when (rest /= []) $ void $ onEventOnce (timeEvent $ head rest) $ executeAndScheduleNext' f sched


--schedule'_ :: [UTCTime] -> Nomex () -> Nomex ()
--schedule'_ ts f = schedule' ts (const f)

-- * Individual events

-- | Build an event firing at a specific time
timeEvent :: UTCTime -> EventM n UTCTime
timeEvent t = SignalEvent $ Signal t

-- | Build a message event, that can be intercepted by another rule
-- this is useful for message-passing style of communication
messageEvent :: (Typeable a, Show a, Eq a) => Msg a -> EventM n a
messageEvent m = SignalEvent m

-- | Build a event firing immediatly, yelding the value of the Nomex
liftEvent :: n a -> EventM n a
liftEvent = LiftEvent

-- * internals

 -- Embed a single Signal as an EventM
signalEvent    :: (Eq s, Typeable s, Show s, Typeable e, Show e, Eq e) => s -> EventM n e
signalEvent = SignalEvent . Signal

-- Embed a single Signal as an EventM
inputEvent    :: (Typeable e, Show e, Eq e) => InputField -> ClientNumber -> EventM n e
inputEvent i cn = SignalEvent $ Signal $ Input i cn


