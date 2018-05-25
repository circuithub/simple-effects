{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, DataKinds, GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- | A neat effect that you can use to get early returns in your functions. Here's how to use it.
--
--   Before:
--
-- @
--   f = do
--       m1 <- maybeFunc1
--       case m1 of
--           Nothing -> return "1 nothing"
--           Just x -> do
--               m2 <- maybeFunc2
--               case m2 of
--                   Nothing -> return "2 nothing"
--                   Just y -> return (x <> y)
-- @
--
--   After:
--
-- @
--   f = handleEarly $ do
--       m1 <- maybeFunc1
--       x <- ifNothingEarlyReturn "1 nothing" m1
--       m2 <- maybeFunc2
--       y <- ifNothingEarlyReturn "2 nothing" m2
--       return (x <> y)
-- @
--
--   You can use the 'earlyReturn' function directly, or one of the helpers for common use cases.
module Control.Effects.Early
    ( module Control.Effects, Early, EffMethods(..)
    , earlyReturn, handleEarly, onlyDo, ifNothingEarlyReturn, ifNothingDo
    , ifLeftEarlyReturn, ifLeftDo ) where

import Import

import Control.Effects

newtype EarlyValue a = EarlyValue { getEarlyValue :: a }
data Early a
instance Effect (Early a) where
    data EffMethods (Early a) m = EarlyMethods
        { _earlyReturn :: forall b. a -> m b }
    liftThrough (EarlyMethods f) = EarlyMethods (lift . f)
    mergeContext m = EarlyMethods (\a -> do
        f <- _earlyReturn <$> m
        f a)
    
instance (Monad m, a ~ b) => MonadEffect (Early a) (ExceptT (EarlyValue b) m) where
    effect = EarlyMethods (throwE . EarlyValue)

-- | Allows you to return early from a function. Make sure you 'handleEarly' to get the actual
--   result out.
earlyReturn :: forall a b m. MonadEffect (Early a) m => a -> m b
EarlyMethods earlyReturn = effect

-- | Get the result from a computation. Either the early returned one, or the regular result.
handleEarly :: Monad m => ExceptT (EarlyValue a) m a -> m a
handleEarly = fmap (either getEarlyValue id)
            . runExceptT

-- | Only do the given action and exit early with it's result.
onlyDo :: MonadEffect (Early a) m => m a -> m b
onlyDo m = m >>= earlyReturn

-- | Early return the given value if the 'Maybe' is 'Nothing'. Otherwise, contnue with the value
--   inside of it.
ifNothingEarlyReturn :: MonadEffect (Early a) m => a -> Maybe b -> m b
ifNothingEarlyReturn a = maybe (earlyReturn a) return

-- | Only do the given action and early return with it's result if the given value is 'Nothing'.
--   Otherwise continue with the value inside of the 'Maybe'.
ifNothingDo :: MonadEffect (Early a) m => m a -> Maybe b -> m b
ifNothingDo m = maybe (onlyDo m) return

-- | If the value is a 'Left', get the value, process it and early return the result.
--   Otherwise just return the 'Right' value.
ifLeftEarlyReturn :: MonadEffect (Early c) m => (a -> c) -> Either a b -> m b
ifLeftEarlyReturn f = either (earlyReturn . f) return

-- | If the value is a 'Left', get the value, process it and only do the resulting action.
--   Otherwise just return the 'Right' value.
ifLeftDo :: MonadEffect (Early c) m => (a -> m c) -> Either a b -> m b
ifLeftDo f = either (onlyDo . f) return
