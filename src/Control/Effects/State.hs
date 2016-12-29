{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs, BangPatterns #-}
module Control.Effects.State (module Control.Effects.State, module Control.Effects1) where

import Interlude hiding (Set, State)

import Data.IORef

import Control.Effects1

data State s
data Get
data Set
data StateMessage s a where
    GetMessage :: StateMessage s Get
    SetMessage :: !s -> StateMessage s Set
data StateResult s a where
    GetResult :: { getGetResult :: !s } -> StateResult s Get
    SetResult :: StateResult s Set

type instance EffectMsg1 (State s) = StateMessage s
type instance EffectRes1 (State s) = StateResult s
type instance EffectCon1 (State s) a = ()

instance Monad m => MonadEffect1 (State s) (StateT s m) where
    effect1 _ GetMessage = GetResult <$> get
    effect1 _ (SetMessage s) = SetResult <$ put s
    {-# INLINE effect1 #-}

type MonadEffectState s m = MonadEffect1 (State s) m

stateEffect :: forall s a m. MonadEffectState s m
            => StateMessage s a -> m (StateResult s a)
stateEffect = effect1 (Proxy :: Proxy (State s))
{-# INLINE stateEffect #-}

getState :: forall s m. MonadEffectState s m => m s
getState = getGetResult <$> stateEffect GetMessage
{-# INLINE getState #-}

setState :: forall s m. MonadEffectState s m => s -> m ()
setState s = void $ stateEffect (SetMessage s)
{-# INLINE setState #-}

modifyState :: forall s m. MonadEffectState s m => (s -> s) -> m ()
modifyState f = do
    s <- getState
    let s' = f s in s' `seq` setState s'
{-# INLINE modifyState #-}

handleState :: forall m s a. Monad m => m s -> (s -> m ())
            -> EffectHandler1 (State s) m a -> m a
handleState getter setter =
    handleEffect1 handler
    where handler :: forall b. StateMessage s b -> m (StateResult s b)
          handler GetMessage = GetResult <$> getter
          handler (SetMessage s) = SetResult <$ setter s
{-# INLINE handleState #-}

handleStateIO :: MonadIO m => s -> EffectHandler1 (State s) m a -> m a
handleStateIO initial m = do
    ref <- liftIO (newIORef initial)
    m & handleState (liftIO (readIORef  ref)) (liftIO . writeIORef ref)
{-# INLINE handleStateIO #-}

handleStateT :: Monad m => s -> StateT s m a -> m a
handleStateT = flip evalStateT
{-# INLINE handleStateT #-}
