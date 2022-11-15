{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module LogPipe.Class
where

import FSD.Prelude
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State.Lazy as State
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Reducible as Reducible
import qualified LogPipe.Common as Common

class (Monad m) => MonadLog m where
    sendLogMessage :: Common.LogMessage -> m ()

instance MonadLog IO where
    sendLogMessage = Common.sendLogMessage

instance
    (Reducible.MonadLifting n m, Monad n, MonadLog m) =>
    MonadLog (Reducible.Lifting n m)
  where
    sendLogMessage = Reducible.applyLifting . sendLogMessage

instance
    {-# OVERLAPPABLE #-}
    (Reducible.Reducible m, Monad m, MonadLog (Reducible.Reduced m)) =>
    MonadLog m
  where
    sendLogMessage msg = Reducible.fromReduced $
        sendLogMessage msg



class (MonadLog m) => MonadLogContext m where
    askLogContext :: m Common.LogContext
    localLogContext :: (Common.LogContext -> Common.LogContext) -> m a -> m a

instance (MonadLogContext m) => MonadLogContext (Reader.ReaderT r m) where
    askLogContext = lift askLogContext
    localLogContext fn act = Reader.ReaderT $ \r ->
        localLogContext fn (Reader.runReaderT act r)

instance
    {-# OVERLAPPING #-}
    (MonadLog m) =>
    MonadLog (Reader.ReaderT Common.LogContext m)
  where
    sendLogMessage (Common.LogMessage msgContext msgText) =
        ReaderT $ \appContext ->
            sendLogMessage $
                Common.LogMessage (appContext <> msgContext) msgText

instance
    {-# OVERLAPPING #-}
    (MonadLog m) =>
    MonadLogContext (Reader.ReaderT Common.LogContext m)
  where
    askLogContext = Reader.ask
    localLogContext = Reader.local

instance (MonadLogContext m) => MonadLogContext (State.StateT s m) where
    askLogContext = lift askLogContext
    localLogContext fn act = State.StateT $ \s ->
        localLogContext fn (State.runStateT act s)

instance (MonadLogContext m) => MonadLogContext (Except.ExceptT e m) where
    askLogContext = lift askLogContext
    localLogContext fn act = Except.ExceptT $
        localLogContext fn (Except.runExceptT act)

instance
    {-# OVERLAPPABLE #-}
    (Reducible.Reducible m, Monad m, MonadLogContext (Reducible.Reduced m)) =>
    MonadLogContext m
  where
    askLogContext = Reducible.fromReduced askLogContext
    localLogContext fn act = Reducible.fromReduced $
        localLogContext fn (Reducible.toReduced act)
