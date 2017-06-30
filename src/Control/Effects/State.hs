{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
{-# LANGUAGE DataKinds, TypeInType #-}
-- | The 'MonadState' you know and love with some differences. First, there's no functional
--   dependency limiting your stack to a single state type. This means less type inference so
--   it might not be enough to just write 'getState'. Write 'getState @MyStateType' instead using
--   TypeApplications.
--
--   Second, the functions have less generic names and are called 'getState' and 'setState'.
--
--   Third, since it's a part of this effect framework, you get a 'handleState' function with
--   which you can provide a different state implementation _at runtime_.
module Control.Effects.State (module Control.Effects.State, module Control.Effects) where

import Import hiding (State)
import Data.IORef

import Control.Effects

data State s = Get | Set
data instance Effect (State s) method mr where
    GetStateMsg :: Effect (State s) 'Get 'Msg
    GetStateRes :: { getGetStateRes :: s } -> Effect (State s) 'Get 'Res
    SetStateMsg :: s -> Effect (State s) 'Set 'Msg
    SetStateRes :: Effect (State s) 'Set 'Res

instance Monad m => MonadEffect (State s) (StateT s m) where
    effect GetStateMsg = GetStateRes <$> get
    effect (SetStateMsg s) = SetStateRes <$ put s
    {-# INLINE effect #-}

getState :: forall s m. MonadEffect (State s) m => m s
getState = getGetStateRes <$> effect GetStateMsg
{-# INLINE getState #-}

setState :: forall s m. MonadEffect (State s) m => s -> m ()
setState s = void $ effect (SetStateMsg s)
{-# INLINE setState #-}

modifyState :: forall s m. MonadEffect (State s) m => (s -> s) -> m ()
modifyState f = do
    s <- getState
    let s' = f s in s' `seq` setState s'
{-# INLINE modifyState #-}

-- | Handle the 'MonadEffect (State s)' constraint by providing custom handling functions.
handleState :: forall m s a. Monad m => m s -> (s -> m ())
            -> EffectHandler (State s) m a -> m a
handleState getter setter =
    handleEffect handler
    where handler :: forall method. Effect (State s) method 'Msg -> m (Effect (State s) method 'Res)
          handler GetStateMsg = GetStateRes <$> getter
          handler (SetStateMsg s) = SetStateRes <$ setter s
{-# INLINE handleState #-}

-- | Handle the state requirement using an 'IORef'.
handleStateIO :: MonadIO m => s -> EffectHandler (State s) m a -> m a
handleStateIO initial m = do
    ref <- liftIO (newIORef initial)
    m & handleState (liftIO (readIORef  ref)) (liftIO . writeIORef ref)
{-# INLINE handleStateIO #-}

-- | Handle the state requirement using the standard 'StateT' transformer.
handleStateT :: Monad m => s -> StateT s m a -> m a
handleStateT = flip evalStateT
{-# INLINE handleStateT #-}
