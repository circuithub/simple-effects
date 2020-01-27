{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
-- | This effect allows you to "throw" a signal. For the most part signals are the same as checked
--   exceptions. The difference here is that the handler has the option to provide the value that
--   will be the result /of calling the 'signal' function/. This effectively allows you to have
--   recoverable exceptions at the throw site, instead of just at the handling site.
module Control.Effects.Signal
    ( ResumeOrBreak(..), Signal(..), throwSignal, handleSignal
    , Throw, handleException, handleToEither, module Control.Effects
    , module Control.Monad.Trans.Except, MaybeT(..), discardAllExceptions, showAllExceptions
    , HandleException(..), handleWithoutDiscarding, handleToEitherRecursive, SomeShowableSignal
    , SomeSignal
    , signal ) where

import Import hiding (liftThrough)
import Control.Monad.Trans.Except
import qualified GHC.TypeLits as TL
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Control.Effects
import Control.Monad.Runnable
import GHC.Generics
import qualified Data.Text as T

newtype Signal a b m = SignalMethods
    { _signal :: a -> m b }
    deriving (Generic)
instance Effect (Signal a b) where

signal :: forall a b m. MonadEffect (Signal a b) m => a -> m b
SignalMethods signal = effect

newtype SomeShowableSignal = SomeShowableSignal { getSomeShowableSignal :: Text }
    deriving (Eq, Ord, Read, Show)

data SomeSignal = SomeSignal

type family UnhandledError a b :: ErrorMessage where
    UnhandledError a Void =
           'TL.Text "Unhandled exception of type " ':<>: 'ShowType a
     ':$$: 'TL.Text "You need to handle all the exceptions before running the computation"
    UnhandledError a b =
           'TL.Text "Unhandled signal of type " ':<>: 'ShowType a
           ':<>: 'TL.Text " expecting a return value of type " ':<>: 'ShowType b
     ':$$: 'TL.Text "You need to handle all the signals before running the computation"

instance TypeError (UnhandledError a b) => MonadEffect (Signal a b) IO where
    effect = undefined

instance {-# INCOHERENT #-}
    (Monad m, b ~ c)
    => MonadEffect (Signal a c) (RuntimeImplemented (Signal a b) m) where
    effect = mergeContext $ RuntimeImplemented (liftThrough <$> ask)

instance {-# INCOHERENT #-}
    (Monad m, Show a)
    => MonadEffect (Signal a b) (RuntimeImplemented (Signal SomeShowableSignal Void) m) where
    effect = SignalMethods (fmap absurd . signal . SomeShowableSignal . T.pack . show)

instance {-# INCOHERENT #-}
    Monad m
    => MonadEffect (Signal a b) (RuntimeImplemented (Signal SomeSignal Void) m) where
    effect = SignalMethods (fmap absurd . signal . const SomeSignal)

type Throw e = Signal e Void

-- | The handle function will return a value of this type.
data ResumeOrBreak b c =
    Resume b -- ^ Give a value to the caller of 'signal' and keep going.
    | Break c -- ^ Continue the execution after the handler. The handler will return this value

-- | Throw a signal with no possible recovery. The handler is forced to only return the 'Break'
--   constructor because it cannot construct a 'Void' value.
--
--   If this function is used along with 'handleAsException', this module behaves like regular
--   checked exceptions.
throwSignal :: MonadEffect (Throw a) m => a -> m b
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
handleSignal :: forall a b c m. Monad m
    => (a -> m (ResumeOrBreak b c))
    -> RuntimeImplemented (Signal a b) (ExceptT c m) c
    -> m c
handleSignal f =
    fmap collapseEither
    . runExceptT
    . implement (SignalMethods h)
    where
    h a = do
        rb <- lift (f a)
        resumeOrBreak return throwE rb

-- | This handler can only behave like a regular exception handler. If used along with 'throwSignal'
--   this module behaves like regular checked exceptions.
handleException :: forall a c m.
    Monad m
    => (a -> m c)
    -> RuntimeImplemented (Signal a Void) (ExceptT a m) c
    -> m c
handleException f =
    either f return
    <=< runExceptT
    . implement (SignalMethods throwE)

-- | See documentation for 'handleException'. This handler gives you an 'Either'.
handleToEither ::
    Monad m
    => RuntimeImplemented (Signal a Void) (ExceptT a m) b
    -> m (Either a b)
handleToEither = handleException (return . Left) . fmap Right

-- | Discard all the 'Throw' and 'Signal' effects. If any exception was thrown
--   the result will be 'Nothing'.
discardAllExceptions ::
    Monad m
    => RuntimeImplemented (Signal SomeException Void) (MaybeT m) a
    -> m (Maybe a)
discardAllExceptions = runMaybeT . implement (SignalMethods (const mzero))

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

-- | Satisfies all the 'Throw' and 'Signal' constraints /if/ they all throw 'Show'able
--   exceptions. The first thrown exception will be shown and returned as a 'Left' result.
showAllExceptions ::
    Monad m
    => RuntimeImplemented (Signal SomeShowableSignal Void) (ExceptT SomeShowableSignal m) a
    -> m (Either Text a)
showAllExceptions =
    fmap (mapLeft getSomeShowableSignal)
    . runExceptT
    . implement (SignalMethods throwE)

newtype HandleException e m = HandleExceptionMethods
    { _handleWithoutDiscarding :: forall a. (e -> m a) -> m a -> m a  }
instance Effect (HandleException e) where
    type CanLift (HandleException e) t = RunnableTrans t
    type Transformation (HandleException e) = Invariant
    emap Invariant{..} (HandleExceptionMethods rec') = HandleExceptionMethods $ \f e -> do
        st <- currentState
        res <- mToN (rec' (\ex -> run (f ex) st) (run e st))
        restoreState res
    liftThrough ::
        forall t m. (CanLift (HandleException e) t, Monad m, Monad (t m))
        => HandleException e m -> HandleException e (t m)
    liftThrough (HandleExceptionMethods rec') = HandleExceptionMethods $ \f e -> do
        st <- currentTransState
        res <- lift (rec' (\ex -> runTransformer (f ex) st) (runTransformer e st))
        restoreTransState res
    mergeContext m = HandleExceptionMethods $ \f ex -> do
        g <- _handleWithoutDiscarding <$> m
        g f ex

-- | Use this function to handle exceptions without discarding the 'Throw' effect.
--   You'll want to use this if you're writing a recursive function. Using the regular handlers
--   in that case will result with infinite types.
--
--   Since this function doesn't discard constraints, you still need to handle the exception on
--   the whole computation.
--
--   Here's a slightly contrived example.
--
-- @
--   data NotFound = NotFound
--   data Tree a = Leaf a | Node (Tree a) (Tree a)
--   data Step = GoLeft | GoRight
--   findIndex :: (Handles NotFound m, Eq a) => a -> Tree a -> m [Step]
--   findIndex x (Leaf a) | x == a    = return []
--                        | otherwise = throwSignal NotFound
--   findIndex x (Node l r) = ((GoLeft :) <$> findIndex x l)
--       & handleWithoutDiscarding (\NotFound -> (GoRight :) <$> findIndex x r)
-- @
--
-- Note: When you finally handle the exception effect, the order in which you handle it and
-- other effects determines whether 'handleWithoutDiscarding' rolls back other effects if an exception
-- occured or it preserves all of them up to the point of the exception.
-- Handling exceptions last and handling them first will produce the former and latter
-- behaviour respectively.
handleWithoutDiscarding ::
    forall e m a. MonadEffect (HandleException e) m => (e -> m a) -> m a -> m a
HandleExceptionMethods handleWithoutDiscarding = effect

instance Monad m => MonadEffect (HandleException e) (ExceptT e m) where
    effect = HandleExceptionMethods $ \f ->
        ExceptT . (either (runExceptT . f) (return . Right) <=< runExceptT)

-- | 'handleToEither' that doesn't discard 'Throws' constraints. See documentation for
--   'handleWithoutDiscarding'.
handleToEitherRecursive :: MonadEffect (HandleException e) m => m a -> m (Either e a)
handleToEitherRecursive = handleWithoutDiscarding (return . Left) . fmap Right
