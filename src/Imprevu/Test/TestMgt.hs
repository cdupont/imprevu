{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Imprevu.Test.TestMgt where

import Control.Monad.State
import Control.Monad.Except
import Control.Applicative
import Control.Lens
import Data.Typeable
import Data.List
import Data.Maybe
import Data.Time
import Imprevu
import Imprevu.Evaluation
import System.IO.Unsafe
import System.Random
import Prelude
import           Debug.Trace.Helpers                 (traceM)

data TestState = TestState {eventInfos :: [EventInfoN TestM],
                            outputs    :: [String],
                            variable  :: Var}
                            deriving Show

newtype TestM a = TestM {unTestM :: StateT TestState IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadState TestState)

-- | stores the variable's data
data Var = forall a . (Typeable a, Show a) =>
   Var { vName :: String,
         vData :: a}

deriving instance (Show Var)

instance MonadError String TestM where
  throwError = undefined
  catchError = undefined

instance EvMgt TestM where
   onEvent         = evOnEvent
   delEvent        = evDelEvent
   getEvents       = error "not implem"
   sendMessage m a = eventsEval $ triggerEvent m a

instance SysMgt TestM where
   getCurrentTime    = return date1 --liftIO Data.Time.getCurrentTime
   getRandomNumber a = liftIO $ randomRIO a

instance VarMgt TestM where
   newVar       v a = modify (\(TestState eis os _) -> (TestState eis os (Var v a))) >> (return $ Just (V v))
   readVar        _ = gets variable >>= (\(Var _ a) -> return $ cast a)
   writeVar (V v) a = modify (\(TestState eis os _) -> (TestState eis os (Var v a))) >> return True
   delVar        _  = return True

eventsEval :: EvaluateN TestM TestState () -> TestM ()
eventsEval eval = do
   s <- get
   let ee = EvalEnv s defaultEvalConf
   let (EvalEnv s' _) = runIdentity $ flip execStateT ee $ do
       res <- runExceptT eval
       case res of
         Right a -> return a
         Left _ -> error $ show "error occured"
   put s'

evalEvents :: TestM a -> EvaluateN TestM TestState a
evalEvents (TestM tio) = do
   ee@(EvalEnv s _) <- get
   let (a, s') = unsafePerformIO $ runStateT tio s
   put ee{_evalEnv = s'}
   return a

evOnEvent :: (Typeable e, Show e) => EventM TestM e -> ((EventNumber, e) -> TestM ()) -> TestM EventNumber
evOnEvent ev h = do
   (TestState evs os vs) <- get
   let en = head $ [1..] \\ (map _eventNumber evs)
   put (TestState ((EventInfo en ev h SActive []) : evs) os vs)
   return en

evDelEvent :: EventNumber -> TestM Bool
evDelEvent en = do
   (TestState evs os vs) <- get
   case find ((== en) . view eventNumber) evs of
      Nothing -> return False
      Just eh -> case _evStatus eh of
         SActive -> do
            put (TestState (replaceWith ((== en) . view eventNumber) eh{_evStatus = SDeleted} evs) os vs)
            return True
         SDeleted -> return False


defaultEvalEnv :: EvalEnvN TestM TestState
defaultEvalEnv = EvalEnv (TestState [] [] (Var "" "")) defaultEvalConf

defaultEvalConf :: EvalConfN TestM TestState
defaultEvalConf = EvalConf getEventsTest setEventsTest evalEvents (error "EvalConf") (flip const)

getEventsTest = eventInfos
setEventsTest eis (TestState _ os vs) = (TestState eis os vs)

execEvent :: (Show s, Typeable s, Show e, Typeable e, Eq s, Eq e) => TestM () -> Signal s e -> e -> [String]
execEvent r f d = execEvents r [(f,d)]

execEvents :: (Show s, Typeable s, Show e, Typeable e, Eq s, Eq e) => TestM () -> [(Signal s e, e)] -> [String]
execEvents r sds = outputs $ execSignals r sds defaultEvalEnv

exec :: TestM () -> [String]
exec r = execEvents r ([] :: [(Signal () (), ())])

putStrLn' :: String -> TestM ()
putStrLn' s = modify (\(TestState is ss vs) -> (TestState is (s:ss) vs))

date1, date2, date3 :: UTCTime
date1 = parse822Time "Tue, 02 Sep 1997 09:00:00 -0400"
date2 = parse822Time "Tue, 02 Sep 1997 10:00:00 -0400"
date3 = parse822Time "Tue, 02 Sep 1997 11:00:00 -0400"

parse822Time :: String -> UTCTime
parse822Time = zonedTimeToUTC
              . fromJust
              . parseTimeM True defaultTimeLocale rfc822DateFormat
