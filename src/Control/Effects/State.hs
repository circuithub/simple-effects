{-# LANGUAGE TypeFamilies, ScopedTypeVariables, FlexibleContexts, Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses, GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | The 'MonadState' you know and love with some differences. First, there's no functional
--   dependency limiting your stack to a single state type. This means less type inference so
--   it might not be enough to just write 'getState'. Write @'getState' \@MyStateType@ instead using
--   @TypeApplications@.
--
--   Second, the functions have less generic names and are called 'getState' and 'setState'.
--
--   Third, since it's a part of this effect framework, you get an 'implement' function with
--   which you can provide a different state implementation /at runtime/.
module Control.Effects.State (module Control.Effects.State, module Control.Effects) where

import Import hiding (State)
import Data.IORef
import GHC.Generics
import Control.Effects

data State s m = StateMethods
    { _getState :: m s
    , _setState :: s -> m () }
    deriving (Generic, Effect)

-- | Get current value of the state with the type 's'.
-- You can use type applications to tell the type checker which type of state you want.
-- @getState \@Int@
getState :: forall s m. MonadEffect (State s) m => m s
-- | Set a new value for the state of type 's'
-- You can use type applications to tell the type checker which type of state you're setting.
-- @setState \@Int 5@
setState :: forall s m. MonadEffect (State s) m => s -> m ()
StateMethods getState setState = effect

-- | Transform the state of type 's' using the given function.
-- You can use type applications to tell the type checker which type of state you're modifying.
-- @modifyState \@Int (+ 1)@
modifyState :: forall s m. MonadEffect (State s) m => (s -> s) -> m ()
modifyState f = do
    s <- getState
    let s' = f s
    s' `seq` setState s'

-- | Implement the state effect via the StateT transformer. If you have a function with a type like
-- @f :: MonadEffect (State Int) m => m ()@ you can use 'implementStateViaStateT' to satisfy the
-- 'MonadEffect' constraint.
--
-- @implementStateViaStateT \@Int 0 f :: Monad m => m ()@
implementStateViaStateT :: forall s m a. Monad m => s -> RuntimeImplemented (State s) (StateT s m) a -> m a
implementStateViaStateT s = flip evalStateT s . implement (StateMethods get put)

-- | Handle the state requirement using an 'IORef'. If you have a function with a type like
-- @f :: MonadEffect (State Int) m => m ()@ you can use 'implementStateViaIORef' to replace the
-- 'MonadEffect' constraint with 'MonadIO'. This is convenient if you already have a 'MonadIO'
-- constraint and you don't want to use the 'StateT' transformer for some reason.
--
-- @implementStateViaIORef \@Int 0 f :: MonadIO m => m ()@
implementStateViaIORef :: forall s m a. MonadIO m => s -> RuntimeImplemented (State s) m a -> m a
implementStateViaIORef initial m = do
    ref <- liftIO (newIORef initial)
    m & implement (StateMethods (liftIO (readIORef  ref)) (liftIO . writeIORef ref))
{-# INLINE implementStateViaIORef #-}
