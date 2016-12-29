{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
module Control.Effects.Early
    ( module Control.Effects, Early
    , earlyReturn, handleEarly, onlyDo, ifNothingEarlyReturn, ifNothingDo ) where

import Interlude
import Control.Monad.Trans.Except

import Control.Effects

data Early a
type instance EffectMsg (Early a) = a
type instance EffectRes (Early a) = Void

instance Monad m => MonadEffect (Early a) (ExceptT a m) where
    effect _ = throwE

-- | Allows you to return early from a function. Make sure you 'handleEarly' to get the actual
--   result out.
earlyReturn :: forall a b m. MonadEffect (Early a) m => a -> m b
earlyReturn = fmap absurd . effect (Proxy :: Proxy (Early a))

collapseEither :: Either a a -> a
collapseEither (Left a) = a
collapseEither (Right a) = a

-- | Get the result from a computation. Either the early returned one, or the regular result.
handleEarly :: Monad m => ExceptT a m a -> m a
handleEarly = fmap collapseEither
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
