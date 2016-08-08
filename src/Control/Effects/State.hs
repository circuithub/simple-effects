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

getState :: forall m s. MonadEffect (GetState s) m => m s
getState = effect (Proxy :: Proxy (GetState s)) ()

setState :: forall m s. MonadEffect (SetState s) m => s -> m ()
setState = effect (Proxy :: Proxy (SetState s))

modifyState :: forall m s. (MonadEffect (GetState s) m, MonadEffect (SetState s) m) => (s -> s) -> m ()
modifyState f = setState . f =<< getState

handleGetState :: Monad m => m s -> EffectHandler (GetState s) m a -> m a
handleGetState = handleEffect . const

handleSetState :: Monad m => (s -> m ()) -> EffectHandler (SetState s) m a -> m a
handleSetState = handleEffect

handleStateIO :: MonadIO m => s
                           -> EffectHandler (GetState s) (EffectHandler (SetState s) m) a
                           -> m a
handleStateIO initial m = do
    ref <- liftIO (newIORef initial)
    m & handleGetState (liftIO  (readIORef  ref))
      & handleSetState (liftIO . writeIORef ref)

handleSubstate :: forall s t m a. (MonadEffect (GetState s) m, MonadEffect (SetState s) m)
               => Lens' s t
               -> t
               -> EffectHandler (GetState t) (EffectHandler (SetState t) m) a
               -> m a
handleSubstate lensST initial m = do
    oldState <- getState
    setState (set lensST initial oldState)
    res <- m & handleGetState (view lensST <$> getState)
             & handleSetState (\s -> do
                 oldState <- getState
                 setState (oldState & lensST .~ s))
    setState oldState
    return res

type MonadEffectState s m = (MonadEffect (GetState s) m, MonadEffect (SetState s) m)
