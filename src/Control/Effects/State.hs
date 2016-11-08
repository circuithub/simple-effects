{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Effects.State (module Control.Effects.State, module Control.Effects) where

import Interlude

import Data.IORef

import Control.Effects

data GetState s
data SetState s

type instance EffectMsg (GetState s) = ()
type instance EffectRes (GetState s) = s

type instance EffectMsg (SetState s) = s
type instance EffectRes (SetState s) = ()

instance Monad m => MonadEffect (GetState s) (StateT s m) where
    effect _ _ = get

instance Monad m => MonadEffect (SetState s) (StateT s m) where
    effect _ = put

type MonadEffectState s m = (MonadEffect (GetState s) m, MonadEffect (SetState s) m)
type EffectHandlerState s m = EffectHandler (GetState s) (EffectHandler (SetState s) m)

getState :: forall s m. MonadEffect (GetState s) m => m s
{-# INLINE getState #-}
getState = effect (Proxy :: Proxy (GetState s)) ()

setState :: forall s m. MonadEffect (SetState s) m => s -> m ()
{-# INLINE setState #-}
setState = effect (Proxy :: Proxy (SetState s))

modifyState :: forall s m. MonadEffectState s m => (s -> s) -> m ()
{-# INLINE modifyState #-}
modifyState f = setState . f =<< getState

handleGetState :: m s -> EffectHandler (GetState s) m a -> m a
{-# INLINE handleGetState #-}
handleGetState = handleEffect . const

handleSetState :: (s -> m ()) -> EffectHandler (SetState s) m a -> m a
{-# INLINE handleSetState #-}
handleSetState = handleEffect

handleState :: Monad m => m s -> (s -> m ()) -> EffectHandlerState s m a -> m a
{-# INLINE handleState #-}
handleState getter setter = handleSetState setter . handleGetState (lift getter)

handleStateIO :: MonadIO m => s -> EffectHandlerState s m a -> m a
{-# INLINE handleStateIO #-}
handleStateIO initial m = do
    ref <- liftIO (newIORef initial)
    m & handleState (liftIO (readIORef  ref)) (liftIO . writeIORef ref)

handleStateT :: Monad m => s -> StateT s m a -> m a
{-# INLINE handleStateT #-}
handleStateT = flip evalStateT
