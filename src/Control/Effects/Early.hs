{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Effects.Early
    ( module Control.Effects, Early
    , earlyReturn, handleEarly, onlyDo, ifNothingEarlyReturn, ifNothingDo
    , ifLeftEarlyReturn, ifLeftDo ) where

import Import

import Control.Effects

newtype Early a = Early { getEarlyReturn :: a }
type instance EffectMsg (Early a) = a
type instance EffectRes (Early a) = Void

instance (Monad m, a ~ b) => MonadEffect (Early a) (ExceptT (Early b) m) where
    effect _ = throwE . Early

-- | Allows you to return early from a function. Make sure you 'handleEarly' to get the actual
--   result out.
earlyReturn :: forall a b m. MonadEffect (Early a) m => a -> m b
earlyReturn = fmap (getEarlyReturn . absurd) . effect (Proxy :: Proxy (Early a))

-- | Get the result from a computation. Either the early returned one, or the regular result.
handleEarly :: Monad m => ExceptT (Early a) m a -> m a
handleEarly = fmap (either getEarlyReturn id)
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
