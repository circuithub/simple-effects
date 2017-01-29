{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, OverloadedStrings, MultiParamTypeClasses, NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Control.Effects.Signal
    ( MonadEffectSignal(..), ResumeOrBreak(..), throwSignal, handleSignal
    , Throws, handleException, handleToEither, module Control.Effects
    , module Control.Monad.Trans.Except, MaybeT(..), discardAllExceptions, showAllExceptions
    , Handles(..), handleToEitherRecursive ) where

import Import
import Control.Monad.Trans.Except
import qualified GHC.TypeLits as TL
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Control.Effects
import Control.Monad.Runnable

data Signal a b
type instance EffectMsg (Signal a b) = a
type instance EffectRes (Signal a b) = b

data SomeSignal = SomeSignal { getSomeSignal :: Text } deriving (Eq, Ord, Read, Show)

type family UnhandledError a b :: ErrorMessage where
    UnhandledError a Void =
           'TL.Text "Unhandled exception of type " ':<>: 'ShowType a
     ':$$: 'TL.Text "You need to handle all the exceptions before running the computation"
    UnhandledError a b =
           'TL.Text "Unhandled signal of type " ':<>: 'ShowType a
           ':<>: 'TL.Text " expecting a return value of type " ':<>: 'ShowType b
     ':$$: 'TL.Text "You need to handle all the signals before running the computation"

instance {-# OVERLAPPABLE #-} Monad m => MonadEffect (Signal e b) (ExceptT e m) where
    effect _ = throwE
instance (Show e, Monad m) => MonadEffect (Signal e b) (ExceptT SomeSignal m) where
    effect _ = throwE . SomeSignal . pack . show
instance Monad m => MonadEffect (Signal a b) (MaybeT m) where
    effect _ _ = mzero
instance TypeError (UnhandledError a b)
      => MonadEffect (Signal a b) IO where
    effect = undefined
-- | This class allows you to "throw" a signal. For the most part signals are the same as checked
--   exceptions. The difference here is that the handler has the option to provide the value that
--   will be the result /of calling the 'signal' function/. This effectively allows you to have
--   recoverable exceptions at the call site, instead of just at the handling site.
--
--   This class can be considered an alias for @'MonadEffect' ('Signal' a b)@ so your code isn't
--   required to provide any instances.
class MonadEffect (Signal a b) m => MonadEffectSignal a b m where
    -- | There are no restrictions on the type of values that can be thrown or returned.
    signal :: a -> m b
    signal = effect (Proxy :: Proxy (Signal a b))

type Throws e m = MonadEffectSignal e Void m

instance (Monad m, b ~ c) => MonadEffectSignal a c (EffectHandler (Signal a b) m)
instance Monad m => MonadEffectSignal a b (MaybeT m)
instance {-# OVERLAPPABLE #-} Monad m => MonadEffectSignal e b (ExceptT e m)
instance (Monad m, Show e) => MonadEffectSignal e b (ExceptT SomeSignal m)
instance MonadEffect (Signal a b) IO => MonadEffectSignal a b IO
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
handleException :: Monad m => (a -> m c) -> ExceptT a m c -> m c
handleException f = either f return <=< runExceptT

-- | See documentation for 'handleException'. This handler gives you an 'Either'.
handleToEither :: ExceptT e m a -> m (Either e a)
handleToEither = runExceptT

-- | Discard all the 'Throws' and 'MonadEffectSignal' constraints. If any exception was thrown
--   the result will be 'Nothing'.
discardAllExceptions :: MaybeT m a -> m (Maybe a)
discardAllExceptions = runMaybeT

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

-- | Satisfies all the 'Throws' and 'MonadEffectSignal' constraints /if/ they all throw 'Show'able
--   exceptions. The first thrown exception will be shown and returned as a 'Left' result.
showAllExceptions :: Functor m => ExceptT SomeSignal m a -> m (Either Text a)
showAllExceptions = fmap (mapLeft getSomeSignal) . runExceptT

-- | A class of monads that throw and catch exceptions of type @e@. An overlappable instance is
--   given so you just need to make sure your transformers have a 'RunnableTrans' instance.
class Throws e m => Handles e m where
    -- | Use this function to handle exceptions without discarding the 'Throws' constraint.
    --   You'll want to use this if you're writing a recursive function. Using the regular handlers
    --   in that case will result with infinite types.
    --
    --   Since this function doesn't discard constraints, you still need to handle the whole thing.
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
    --       & handleRecursive (\NotFound -> (GoRight :) <$> findIndex x r)
    -- @
    --
    -- Note: When you finally handle the exception effect, the order in which you handle it and
    -- other effects determines whether 'handleRecursive' rolls back other effects if an exception
    -- occured or it preserves all of them up to the point of the exception.
    -- Handling exceptions last and handling them first will produce the former and latter
    -- behaviour respectively.
    handleRecursive :: (e -> m a) -> m a -> m a

instance Monad m => Handles e (ExceptT e m) where
    handleRecursive f = ExceptT . (either (runExceptT . f) (return . Right) <=< runExceptT)
    {-# INLINE handleRecursive #-}

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), Handles e m, RunnableTrans t)
         => Handles e (t m) where
    handleRecursive f e = do
        st <- currentTransState
        res <- lift (handleRecursive (\ex -> runTransformer (f ex) st) (runTransformer e st))
        restoreTransState res
    {-# INLINE handleRecursive #-}

-- | 'handleToEither' that doesn't discard 'Throws' constraints. See documentation for
--   'handleRecursive'.
handleToEitherRecursive :: Handles e m => m a -> m (Either e a)
handleToEitherRecursive = handleRecursive (return . Left) . fmap Right
