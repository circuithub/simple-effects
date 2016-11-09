{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, OverloadedStrings, MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Control.Effects.Signal
    ( MonadEffectSignal(..), ResumeOrBreak(..), throwSignal, handleSignal, handleAsException
    , Throws, handleException, handleToEither, module Control.Effects
    , module Control.Monad.Trans.Except, MaybeT(..), discardAllExceptions, showAllExceptions ) where

import Interlude
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe

import Control.Effects

data Signal a b
type instance EffectMsg (Signal a b) = a
type instance EffectRes (Signal a b) = b

data SomeSignal = SomeSignal { getSomeSignal :: Text } deriving (Eq, Ord, Read, Show)

instance {-# OVERLAPPABLE #-} Monad m => MonadEffect (Signal e Void) (ExceptT e m) where
    effect _ = throwE
instance (Show e, Monad m) => MonadEffect (Signal e Void) (ExceptT SomeSignal m) where
    effect _ = throwE . SomeSignal . pshow
instance Monad m => MonadEffect (Signal a Void) (MaybeT m) where
    effect _ _ = mzero

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

type Throws e m = MonadEffectSignal e Void m

instance Monad m => MonadEffectSignal a b (EffectHandler (Signal a b) m)
instance Monad m => MonadEffectSignal a Void (MaybeT m)
instance {-# OVERLAPPABLE #-} Monad m => MonadEffectSignal e Void (ExceptT e m)
instance (Monad m, Show e) => MonadEffectSignal e Void (ExceptT SomeSignal m)
instance {-# OVERLAPPABLE #-} (MonadEffectSignal a b m, MonadTrans t, Monad (t m))
         => MonadEffectSignal a b (t m)

-- | The handle function will return a value of this type.
data ResumeOrBreak b c = Resume b -- ^ Give a value to the caller of 'signal' and keep going.
                       | Break c -- ^ Continue the execution after the handler. The handler will
                                 --   return this value

-- | Throw a signal with no possible recovery. The handler is forced to only return the 'Break'
--   constructor because it cannot construct a 'Void' value.
--
--   If this function is used along with 'handleAsException', this module behaves like regular
--   checked exceptions.
throwSignal :: Throws a m => a -> m b
throwSignal = fmap absurd . signal

resumeOrBreak :: (b -> a) -> (c -> a) -> ResumeOrBreak b c -> a
resumeOrBreak ba _  (Resume b) = ba b
resumeOrBreak _  ca (Break c)  = ca c

collapseEither :: Either a a -> a
collapseEither (Left a) = a
collapseEither (Right a) = a

-- | Handle signals of a computation. The handler function has the option to provide a value
--   to the caller of 'signal' and continue execution there, or do what regular exception handlers
--   do and continue execution after the handler.
handleSignal :: Monad m
             => (a -> m (ResumeOrBreak b c))
             -> EffectHandler (Signal a b) (ExceptT c m) c
             -> m c
handleSignal f = fmap collapseEither
               . runExceptT
               . handleEffect (resumeOrBreak return throwE <=< lift . f)

-- | This handler can only behave like a regular exception handler. If used along with 'throwSignal'
--   this module behaves like regular checked exceptions.
handleAsException :: Monad m
                  => (a -> m c)
                  -> EffectHandler (Signal a b) (ExceptT c m) c
                  -> m c
handleAsException f = handleSignal (fmap Break . f)

-- | In case only 'throwSignal' is used then the function signatures will have a @'Throws' a m@
--   constraint or, equivalently, a @'MonadEffectSignal' a Void m@ constraint. In those cases you
--   can use this function to handle their exceptions. This function will not work for handing other
--   signals because 'ExceptT' doesn't satisfy other constraints.
--
--   The advantage of using this handler is that your inferred transformer stack will have one less
--   layer which can potentially lead to slight performance increases.
handleException :: Monad m => (a -> m c) -> ExceptT a m c -> m c
handleException f = either f return <=< runExceptT

-- | See documentation for 'handleException'. This handler gives you an 'Either'.
handleToEither :: ExceptT e m a -> m (Either e a)
handleToEither = runExceptT

-- | Discard all the 'Throws' constraints. If any exception was thrown the result will be
--   'Nothing'.
discardAllExceptions :: MaybeT m a -> m (Maybe a)
discardAllExceptions = runMaybeT

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

-- | Satisfies all the 'Throws' constraints _if_ they all throw 'Show'able exceptions. The first
--   thrown exception will be shown and returned as a 'Left' result.
showAllExceptions :: Functor m => ExceptT SomeSignal m a -> m (Either Text a)
showAllExceptions = fmap (mapLeft getSomeSignal) . runExceptT
