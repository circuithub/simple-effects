{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-| The 'Async' effect allows you to fork new threads in monads other than just 'IO'.
-}
module Control.Effects.Async where

import Import
import Control.Effects
import qualified Control.Concurrent.Async as Async
import Control.Monad.Runnable
import Control.Effects.State

-- | The name of the effect
data Async

-- | The type that represents the forked computation in the monad @m@ that eventually computes
--   a value of type @a@. Depending on the monad, the computation may produce zero, one or even
--   multiple values of that type.
type AsyncThread m a = Async.Async (m a)

instance Effect Async where
    data EffMethods Async m = AsyncMethods
        { _async :: forall a. m a -> m (AsyncThread m a)
        , _waitAsync :: forall a. AsyncThread m a -> m a }
    type CanLift Async t = RunnableTrans t
    mergeContext mm = AsyncMethods
        (\a -> mm >>= ($ a) . _async)
        (\a -> mm >>= ($ a) . _waitAsync)
    liftThrough (AsyncMethods f g) = AsyncMethods
        (\tma -> do
            st <- currentTransState
            !res <- lift (f (runTransformer tma st))
            return $ fmap (lift >=> restoreTransState) res
            )
        (\a -> do
            st <- currentTransState
            res <- lift (g (fmap (`runTransformer` st) a))
            restoreTransState res
            )

-- | The 'IO' implementation uses the @async@ library.
instance MonadEffect Async IO where
    effect = AsyncMethods (fmap (fmap return) . Async.async) (join . Async.wait)

-- | Fork a new thread to run the given computation. The monadic context is forked into the new
--   thread.
--
--   For example, if we use state, the current state value will be visible int he forked computation.
--   Depending on how we ultimately implement the state, modifying it may or may not be visible
--   from the main thread. If we use 'implementStateViaStateT' then setting the state in the forked
--   thread will just modify the thread-local value. On the other hand, if we use
--  'implementStateViaIORef' then both the main thread and the new thread will use the same reference
--   meaning they can interact through it.
async :: MonadEffect Async m => m a -> m (AsyncThread m a)

-- | Wait for the thread to finish and return it's result. The monadic context will also be merged.
--
--   Example:
--
-- @
--  'setState' \@Int 1
--  th <- 'async' $ do
--      'setState' \@Int 2
--  'waitAsync' th
--  print =<< 'getState' \@Int -- Outputs 2
-- @
waitAsync :: MonadEffect Async m => AsyncThread m a -> m a
AsyncMethods async waitAsync = effect

-- | This will discard the @'MonadEffect' 'Async' m@ constraint by forcing @m@ to be 'IO'.
--   The functions doesn't actually do anything, the real implementation is given by the
--   @'MonadEffect' 'Async' IO@ instance which uses the @async@ package.
implementAsyncViaIO :: IO a -> IO a
implementAsyncViaIO = id