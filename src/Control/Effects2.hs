{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes, TypeApplications, RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, InstanceSigs, UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction, ConstraintKinds, DeriveGeneric, DefaultSignatures #-}
module Control.Effects2 (module Control.Effects2) where

import Control.Monad.Trans
import Data.Proxy
import Control.Monad
import Control.Applicative
import Control.Monad.Runnable
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import GHC.Generics
import Control.Monad.Trans.Reader
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
    deriving (Functor, Applicative, Monad, MonadPlus, Alternative)

instance MonadTrans (RuntimeImplementation e) where
    lift = RuntimeImplementation . lift

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

data State s

instance Effect (State s) where
    data EffMethods (State s) m = StateMethods
        { _getState :: m s
        , _setState :: s -> m () }
        deriving (Generic)

getState :: forall s m. MonadEffect (State s) m => m s
setState :: forall s m. MonadEffect (State s) m => s -> m ()
StateMethods{_getState = getState, _setState = setState} = effect

-- instance Monad m => MonadEffect (State s) (StateT s m) where
--     effect = StateMethods get put

-- implementStateViaStateT :: Monad m => s -> StateT s m a -> m a
-- implementStateViaStateT = flip evalStateT 

test = implement @(State Int) (StateMethods (return 0) (const (return ()))) $ do
    (x :: Int) <- getState
    setState (x + 1)
    getState @Int

-- data List

-- instance Effect List where
--     data EffMethods List m = ListMethods
--         { choose :: forall a. [a] -> m a }
--         deriving (Generic)