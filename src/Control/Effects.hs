{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor
           , GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving #-}
module Control.Effects where

import Interlude

import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base

type family EffectMsg eff :: *
type family EffectRes eff :: *

class Monad m => MonadEffect eff m where
    -- | Use the effect described by 'eff'.
    effect :: proxy eff -> EffectMsg eff -> m (EffectRes eff)

-- | The 'EffectHandler' is rally just a 'ReaderT' carrying around the function that knows how to
--   handle the effect.
newtype EffectHandler eff m a = EffectHandler
    { unpackEffectHandler :: ReaderT (EffectMsg eff -> m (EffectRes eff)) m a }
    deriving (Functor, Applicative, Monad, MonadState s, MonadIO, MonadCatch, MonadThrow, MonadRandom)

instance MonadTrans (EffectHandler eff) where
    lift = EffectHandler . lift

instance MonadReader s m => MonadReader s (EffectHandler eff m) where
    ask = EffectHandler (lift ask)
    local f (EffectHandler rdr) = EffectHandler (ReaderT $ local f . runReaderT rdr)

deriving instance MonadBase IO m => MonadBase IO (EffectHandler eff m)

instance MonadBaseControl IO m => MonadBaseControl IO (EffectHandler eff m) where
    type StM (EffectHandler eff m) a = StM (ReaderT (EffectMsg eff -> m (EffectRes eff)) m) a
    liftBaseWith f = EffectHandler $ liftBaseWith $ \q -> f (q . unpackEffectHandler)
    restoreM = EffectHandler . restoreM

instance {-# OVERLAPPABLE #-} (MonadEffect eff m, MonadTrans t, Monad (t m))
         => MonadEffect eff (t m) where
    effect p msg = lift (effect p msg)

instance Monad m => MonadEffect eff (EffectHandler eff m) where
    effect _ msg = EffectHandler (ReaderT ($ msg))

-- | Handle the effect described by 'eff'.
handleEffect :: Monad m => (EffectMsg eff -> m (EffectRes eff)) -> EffectHandler eff m a -> m a
handleEffect f eh = runReaderT (unpackEffectHandler eh) f
