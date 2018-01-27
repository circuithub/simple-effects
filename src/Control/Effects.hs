{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeApplications, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving, DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Control.Effects (module Control.Effects) where

import Import hiding (liftThrough)
import Control.Monad.Runnable
import Control.Effects.Generic

class Effect e where
    data EffMethods e (m :: * -> *) :: *
    liftThrough ::
        forall t m. (RunnableTrans t, Monad m, Monad (t m))
        => (Proxy e, Proxy m, Proxy t) -> EffMethods e m -> EffMethods e (t m)
    default liftThrough ::
        forall t m. (RunnableTrans t, Monad m, Monad (t m), SimpleMethods (EffMethods e) m t)
        => (Proxy e, Proxy m, Proxy t) -> EffMethods e m -> EffMethods e (t m)
    liftThrough = genericLiftThrough
    
    mergeContext :: Monad m => m (EffMethods e m) -> EffMethods e m
    default mergeContext :: MonadicMethods (EffMethods e) m => m (EffMethods e m) -> EffMethods e m
    mergeContext = genericMergeContext

class (Effect e, Monad m) => MonadEffect e m where
    effect :: EffMethods e m

instance {-# OVERLAPPABLE #-}
    (MonadEffect e m, Monad (t m), RunnableTrans t)
    => MonadEffect e (t m) where
    effect = liftThrough (Proxy @e, Proxy @m, Proxy @t) effect

newtype RuntimeImplementation e m a = RuntimeImplementation { getRuntimeImplementation :: ReaderT (EffMethods e m) m a }
    deriving 
        (Functor, Applicative, Monad, MonadPlus, Alternative, MonadState s, MonadIO, MonadCatch
        , MonadThrow, MonadRandom )

instance MonadTrans (RuntimeImplementation e) where
    lift = RuntimeImplementation . lift

instance MonadReader r m => MonadReader r (RuntimeImplementation e m) where
    ask = RuntimeImplementation (lift ask)
    local f (RuntimeImplementation rdr) = RuntimeImplementation (ReaderT (local f . runReaderT rdr))

deriving instance MonadBase b m => MonadBase b (RuntimeImplementation e m)
instance MonadBaseControl b m => MonadBaseControl b (RuntimeImplementation e m) where
    type StM (RuntimeImplementation e m) a = StM (ReaderT (EffMethods e m) m) a
    liftBaseWith f = RuntimeImplementation $ liftBaseWith $ \q -> f (q . getRuntimeImplementation)
    restoreM = RuntimeImplementation . restoreM

instance RunnableTrans (RuntimeImplementation e) where
    type TransformerResult (RuntimeImplementation e) m a = a
    type TransformerState (RuntimeImplementation e) m = EffMethods e m
    currentTransState = RuntimeImplementation ask
    restoreTransState = return
    runTransformer (RuntimeImplementation m) = runReaderT m

instance (Effect e, Monad m) => MonadEffect e (RuntimeImplementation e m) where
    effect = mergeContext $ RuntimeImplementation (liftThrough (Proxy, Proxy, Proxy) <$> ask)

implement :: forall e m a. EffMethods e m -> RuntimeImplementation e m a -> m a
implement em (RuntimeImplementation r) = runReaderT r em

type family MonadEffects effs m :: Constraint where
    MonadEffects '[] m = ()
    MonadEffects (eff ': effs) m = (MonadEffect eff m, MonadEffects effs m)
