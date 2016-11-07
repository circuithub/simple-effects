{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.Effects.Signal
    ( MonadEffectSignal(..), ResumeOrBreak(..), throwSignal, handleSignal, handleAsException
    , module Control.Effects ) where

import Interlude
import Prelude (Show(..))

import Control.Effects

data Signal a b
type instance EffectMsg (Signal a b) = a
type instance EffectRes (Signal a b) = b

-- | This class allows you to "throw" a signal. For the most part signals are the same as checked
--   exceptions. The difference here is that the handler has the option to provide the value that
--   will be the result _of calling the signal function_. This effectibvely allows you to have
--   recoverable exceptions at the call site, instead of just at the handling site.
--
--   This class can be considered an alias for @MonadEffect (Signal a b)@ so your code isn't
--   required to provide any instances.
class MonadEffect (Signal a b) m => MonadEffectSignal a b m | m a -> b where
    -- | There are no restrictions on the type of values that can be thrown or returned.
    signal :: a -> m b
    signal = effect (Proxy :: Proxy (Signal a b))

instance Monad m => MonadEffectSignal a b (EffectHandler (Signal a b) m)
instance {-# OVERLAPPABLE #-} (MonadEffectSignal a b m, MonadTrans t, Monad (t m))
         => MonadEffectSignal a b (t m)

-- | The handle function will return a value of this type.
data ResumeOrBreak b c = Resume b -- ^ Give a value to the caller of @signal@ and keep going.
                       | Break c -- ^ Continue the execution after the handler. The handler will
                                 --   return this value

data NoResume a = NoResume a deriving (Eq, Ord)
instance Show (NoResume a) where
    show = const "NoResume"
instance Typeable a => Exception (NoResume a)

-- | Throw a signal with no possible recovery. The handler is forced to only return the @Break@
--   constructor because it cannot construct a @Void@ value.
--
--   If this function is used along with @handleAsException@, this module behaves like regular
--   checked exceptions.
throwSignal :: MonadEffectSignal a Void m => a -> m b
throwSignal = fmap absurd . signal

-- | Handle signals of a computation. The handler function has the option to provide a value
--   to the caller of @signal@ and continue execution there, or do what regular exception handlers
--   do and continue execution after the handler.
handleSignal :: (MonadCatch m, Typeable c)
             => (a -> m (ResumeOrBreak b c)) -> EffectHandler (Signal a b) m c -> m c
handleSignal f a =
    handle (\(NoResume c) -> return c) $ handleEffect (\s -> do
        rb <- f s
        case rb of
            Resume b -> return b
            Break c -> throwM (NoResume c)) a

-- | This handler can only behave like a regular exception handler. If used along with @throwSignal@
--   this module behaves like regular checked exceptions.
handleAsException :: (MonadCatch m, Typeable c)
                => (a -> m c) -> EffectHandler (Signal a b) m c -> m c
handleAsException f = handleSignal (fmap Break . f)
