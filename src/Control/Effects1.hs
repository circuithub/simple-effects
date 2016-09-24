{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor
           , GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving
           , IncoherentInstances, RankNTypes, ConstraintKinds #-}
module Control.Effects1 where

import Interlude

import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base

type family EffectMsg1 eff :: * -> *
type family EffectRes1 eff :: * -> *
type family EffectCon1 eff a :: Constraint

class Monad m => MonadEffect1 eff m where
    -- | Use the effect described by 'eff'.
    effect1 :: EffectCon1 eff a => proxy eff -> EffectMsg1 eff a -> m (EffectRes1 eff a)

newtype EffHandling1 eff m = EffHandling1 {
    getHandling1 :: forall a. EffectCon1 eff a => EffectMsg1 eff a -> m (EffectRes1 eff a) }

-- | The 'EffectHandler1' is rally just a 'ReaderT' carrying around the function that knows how to
--   handle the effect.
newtype EffectHandler1 eff m a = EffectHandler1
    { unpackEffectHandler1 :: ReaderT (EffHandling1 eff m) m a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO, MonadCatch, MonadThrow, MonadRandom)

instance MonadTrans (EffectHandler1 eff) where
    lift = EffectHandler1 . lift

instance MonadReader s m => MonadReader s (EffectHandler1 eff m) where
    ask = EffectHandler1 (lift ask)
    local f (EffectHandler1 rdr) = EffectHandler1 (ReaderT $ local f . runReaderT rdr)

deriving instance MonadBase IO m => MonadBase IO (EffectHandler1 eff m)

instance MonadBaseControl IO m => MonadBaseControl IO (EffectHandler1 eff m) where
    type StM (EffectHandler1 eff m) a = StM (ReaderT (EffHandling1 eff m) m) a
    liftBaseWith f = EffectHandler1 $ liftBaseWith $ \q -> f (q . unpackEffectHandler1)
    restoreM = EffectHandler1 . restoreM

instance {-# OVERLAPPABLE #-} (MonadEffect1 eff m, MonadTrans t, Monad (t m))
         => MonadEffect1 eff (t m) where
    effect1 p msg = lift (effect1 p msg)

instance Monad m => MonadEffect1 eff (EffectHandler1 eff m) where
    effect1 _ msg = EffectHandler1 (ReaderT (($ msg) . getHandling1))

-- | Handle the effect described by 'eff'.
handleEffect1 :: Monad m => (forall a. EffectCon1 eff a => EffectMsg1 eff a -> m (EffectRes1 eff a))
             -> EffectHandler1 eff m a -> m a
handleEffect1 f eh = runReaderT (unpackEffectHandler1 eh) (EffHandling1 f)
