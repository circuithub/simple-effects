{-# LANGUAGE TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, PolyKinds #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, DataKinds, NoMonomorphismRestriction #-}
-- | Sometimes it's useful to give a new name to an already existing effect. This module provides
--   the tools to make that easy to do.
module Control.Effects.Newtype where

import Import (ask)
import Control.Effects
import Data.Coerce
import Control.Effects.State

-- | If we have a computation using some effect @original@, we can convert it into a computation
--   that uses the effect @newtyped@ instead. Provided, of course, that @newtyped@ is really a
--   newtype over the @original@ effect.
--
-- @
-- f :: 'MonadEffect' ('State' Int) m => m ()
-- f = getState >>= \i -> setState (i + 1)
--
-- newtype MyState m = MyState ('State' Int m)
--
-- -- inferred: g :: 'MonadEffect' MyState m => m ()
-- g = 'effectAsNewtype' \@MyState \@('State' Int) f
-- @
effectAsNewtype :: forall newtyped original m a.
    (MonadEffect newtyped m, Coercible (newtyped m) (original m))
    => RuntimeImplemented original m a -> m a
effectAsNewtype = implement (coerce (effect @newtyped :: newtyped m))

-- | A useful newtype for any effect. Just provide a unique tag, like a type level string.
newtype EffTag (tag :: k) e (m :: * -> *) = EffTag (e m)
instance Effect e => Effect (EffTag tag e) where
    type CanLift (EffTag tag e) t = CanLift e t
    liftThrough (EffTag e) = EffTag (liftThrough e)
    mergeContext m = EffTag (mergeContext (fmap coerce m))

instance {-# INCOHERENT #-}
    ( e ~ e', Effect e, Monad m
    , CanLift e (RuntimeImplemented (EffTag tag e)) )
    => MonadEffect (EffTag tag e) (RuntimeImplemented (EffTag tag e') m) where
    effect = mergeContext $ RuntimeImplemented (liftThrough <$> ask)

-- | Rename an effect without explicitly declaring a new newtype. Just provide a tag.
--   This is useful if you have two functions using the same effect that you want to combine but
--   you don't want their effects to interact. For example, maybe they both work with @Int@ states
--   but you don't want them to modify each other's number.
tagEffect :: forall tag original m a.
    MonadEffect (EffTag tag original) m
    => RuntimeImplemented original m a -> m a
tagEffect = effectAsNewtype @(EffTag tag original)

-- | Once you tag your effect, it's /slightly/ inconvenient that you have to wrap your implementation
--   when you want to handle it. This function doees the wrapping for you.
--
-- @
-- f :: 'MonadEffect' ('State' Int) m => m ()
-- f = 'getState' >>= \\s -> 'setState' (s * 2)
--
-- g :: 'MonadEffect' ('State' Int) m => m ()
-- g = 'getState' >>= \\s -> 'setState' (s * 3)
--
-- combine :: Monad m => m Int
-- combine =
--     'implementStateViaStateT' 5 $ 'implementTagged' \@"s2" ('StateMethods' 'getState' 'setState')
--     $ 'implementStateViaStateT' 0 $ 'implementTagged' \@"s1" ('StateMethods' 'getState' 'setState')
--     $ do
--     r1 \<- 'tagEffect' \@"s1" \@('State' Int) (f >> 'getState')
--     r2 \<- 'tagEffect' \@"s2" \@('State' Int) (g >> 'getState')
--     return (r1 + r2) -- results in 15
-- @
implementTagged :: forall tag original m a.
    original m -> RuntimeImplemented (EffTag tag original) m a -> m a
implementTagged = implement . coerce
