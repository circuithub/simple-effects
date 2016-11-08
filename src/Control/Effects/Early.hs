{-# LANGUAGE RankNTypes, TypeFamilies, FlexibleContexts, ScopedTypeVariables, MultiParamTypeClasses #-}
module Control.Effects.Early (module Control.Effects, Early, earlyReturn, handleEarly) where

import Interlude
import Control.Monad.Trans.Except

import Control.Effects

data Early a
type instance EffectMsg (Early a) = a
type instance EffectRes (Early a) = Void

instance Monad m => MonadEffect (Early a) (ExceptT a m) where
    effect _ = throwE

-- | Allows you to return early from a function. Make sure you 'handleEarly' to get the actual
--   result out
earlyReturn :: forall a b m. MonadEffect (Early a) m => a -> m b
earlyReturn = fmap absurd . effect (Proxy :: Proxy (Early a))

collapseEither :: Either a a -> a
collapseEither (Left a) = a
collapseEither (Right a) = a

-- | Get the result from a computation. Either the early returned one, or the regular result.
handleEarly :: Monad m => ExceptT a m a -> m a
handleEarly = fmap collapseEither
            . runExceptT
