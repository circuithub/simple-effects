{-# LANGUAGE TypeFamilies, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving, DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Control.Effects (module Control.Effects) where

import Import hiding (liftThrough)
import Control.Monad.Runnable
import Control.Effects.Generic
import GHC.Generics

class Effect e where
    data EffMethods e (m :: * -> *) :: *
    type CanLift e (t :: (* -> *) -> * -> *) :: Constraint
    type CanLift e t = MonadTrans t
    liftThrough ::
        forall t m. (CanLift e t, Monad m, Monad (t m))
        => EffMethods e m -> EffMethods e (t m)
    default liftThrough ::
        forall t m. 
        ( Generic (EffMethods e m), MonadTrans t, Monad m, Monad (t m)
        , SimpleMethods (EffMethods e) m t )
        => EffMethods e m -> EffMethods e (t m)
    liftThrough = genericLiftThrough
    
    mergeContext :: Monad m => m (EffMethods e m) -> EffMethods e m
    default mergeContext :: 
        (Generic (EffMethods e m), MonadicMethods (EffMethods e) m) 
        => m (EffMethods e m) -> EffMethods e m
    mergeContext = genericMergeContext

class (Effect e, Monad m) => MonadEffect e m where
    effect :: EffMethods e m

instance {-# OVERLAPPABLE #-}
    (MonadEffect e m, Monad (t m), CanLift e t)
    => MonadEffect e (t m) where
    effect = liftThrough effect

newtype RuntimeImplemented e m a = RuntimeImplemented 
    { getRuntimeImplemented :: ReaderT (EffMethods e m) m a }
    deriving 
        (Functor, Applicative, Monad, MonadPlus, Alternative, MonadState s, MonadIO, MonadCatch
        , MonadThrow, MonadRandom )

instance MonadTrans (RuntimeImplemented e) where
    lift = RuntimeImplemented . lift

instance MonadReader r m => MonadReader r (RuntimeImplemented e m) where
    ask = RuntimeImplemented (lift ask)
    local f (RuntimeImplemented rdr) = RuntimeImplemented (ReaderT (local f . runReaderT rdr))

deriving instance MonadBase b m => MonadBase b (RuntimeImplemented e m)
instance MonadBaseControl b m => MonadBaseControl b (RuntimeImplemented e m) where
    type StM (RuntimeImplemented e m) a = StM (ReaderT (EffMethods e m) m) a
    liftBaseWith f = RuntimeImplemented $ liftBaseWith $ \q -> f (q . getRuntimeImplemented)
    restoreM = RuntimeImplemented . restoreM

instance RunnableTrans (RuntimeImplemented e) where
    type TransformerResult (RuntimeImplemented e) m a = a
    type TransformerState (RuntimeImplemented e) m = EffMethods e m
    currentTransState = RuntimeImplemented ask
    restoreTransState = return
    runTransformer (RuntimeImplemented m) = runReaderT m

instance (Effect e, Monad m, CanLift e (RuntimeImplemented e)) 
    => MonadEffect e (RuntimeImplemented e m) where
    effect = mergeContext $ RuntimeImplemented (liftThrough <$> ask)

implement :: forall e m a. EffMethods e m -> RuntimeImplemented e m a -> m a
implement em (RuntimeImplemented r) = runReaderT r em

type family MonadEffects effs m :: Constraint where
    MonadEffects '[] m = ()
    MonadEffects (eff ': effs) m = (MonadEffect eff m, MonadEffects effs m)
