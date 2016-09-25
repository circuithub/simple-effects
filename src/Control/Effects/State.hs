{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
module Control.Effects.State (module Control.Effects.State, module Control.Effects) where

import Interlude

import Data.IORef
import Control.Lens

import Control.Effects

data GetState s
data SetState s

type instance EffectMsg (GetState s) = ()
type instance EffectRes (GetState s) = s

type instance EffectMsg (SetState s) = s
type instance EffectRes (SetState s) = ()

type MonadEffectState s m = (MonadEffect (GetState s) m, MonadEffect (SetState s) m)
type EffectHandlerState s m = EffectHandler (GetState s) (EffectHandler (SetState s) m)

{-# INLINE getState #-}
getState :: forall s m. MonadEffect (GetState s) m => m s
getState = effect (Proxy :: Proxy (GetState s)) ()

{-# INLINE setState #-}
setState :: forall s m. MonadEffect (SetState s) m => s -> m ()
setState = effect (Proxy :: Proxy (SetState s))

{-# INLINE modifyState #-}
modifyState :: forall s m. MonadEffectState s m => (s -> s) -> m ()
modifyState f = setState . f =<< getState

{-# INLINE handleGetState #-}
handleGetState :: Monad m => m s -> EffectHandler (GetState s) m a -> m a
handleGetState = handleEffect . const

{-# INLINE handleSetState #-}
handleSetState :: Monad m => (s -> m ()) -> EffectHandler (SetState s) m a -> m a
handleSetState = handleEffect

{-# INLINE handleState #-}
handleState :: Monad m => m s -> (s -> m ()) -> EffectHandlerState s m a -> m a
handleState getter setter = handleSetState setter . handleGetState (lift getter)

handleStateIO :: MonadIO m => s -> EffectHandlerState s m a -> m a
handleStateIO initial m = do
    ref <- liftIO (newIORef initial)
    m & handleState (liftIO (readIORef  ref)) (liftIO . writeIORef ref)

{-# INLINE handleStateT #-}
handleStateT :: Monad m => s -> EffectHandlerState s (StateT s m) a -> m a
handleStateT initial m = evalStateT (handleSetState put $ handleGetState get m) initial

handleSubstate :: MonadEffectState s m => Lens' s t -> t -> EffectHandlerState t m a -> m a
handleSubstate lensST initial m = do
    oldState <- getState
    setState (set lensST initial oldState)
    res <- m & handleGetState (view lensST <$> getState)
             & handleSetState (\s -> do
                 oldState <- getState
                 setState (oldState & lensST .~ s))
    setState oldState
    return res
