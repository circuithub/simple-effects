{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-| The 'Async' effect allows you to fork new threads in monads other than just 'IO'.
-}
module Control.Effects.Async where

import Import
import Control.Effects
import qualified Control.Concurrent.Async as Async
import Control.Monad.Runnable

data Async thread m = AsyncMethods
    { _async :: forall a. m a -> m (thread m a)
    , _waitAsync :: forall a. thread m a -> m a }

class ThreadIdentifier thread where
    mapThread :: (m a -> n b) -> thread m a -> thread n b

instance ThreadIdentifier thread => Effect (Async thread) where
    type CanLift (Async thread) t = RunnableTrans t
    type ExtraConstraint (Async thread) m = UniqueEffect Async m thread
    mergeContext mm = AsyncMethods
        (\a -> mm >>= ($ a) . _async)
        (\a -> mm >>= ($ a) . _waitAsync)
    liftThrough (AsyncMethods f g) = AsyncMethods
        (\tma -> do
            st <- currentTransState
            !res <- lift (f (runTransformer tma st))
            return $ mapThread (lift >=> restoreTransState) res
            )
        (\a -> do
            st <- currentTransState
            res <- lift (g (mapThread (`runTransformer` st) a))
            restoreTransState res
            )

-- | Fork a new thread to run the given computation. The monadic context is forked into the new
--   thread.
--
--   For example, if we use state, the current state value will be visible in the forked computation.
--   Depending on how we ultimately implement the state, modifying it may or may not be visible
--   from the main thread. If we use 'implementStateViaStateT' then setting the state in the forked
--   thread will just modify the thread-local value. On the other hand, if we use
--  'implementStateViaIORef' then both the main thread and the new thread will use the same reference
--   meaning they can interact through it.
async :: MonadEffect (Async thread) m => m a -> m (thread m a)

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
waitAsync :: MonadEffect (Async thread) m => thread m a -> m a
AsyncMethods async waitAsync = effect

-- | The type that represents the forked computation in the monad @m@ that eventually computes
--   a value of type @a@. Depending on the monad, the computation may produce zero, one or even
--   multiple values of that type.
newtype AsyncThread m a = AsyncThread (Async.Async (m a))
    deriving (Functor, Eq, Ord)
instance ThreadIdentifier AsyncThread where
    mapThread f (AsyncThread as) = AsyncThread (fmap f as)

instance UniqueEffect Async (RuntimeImplemented (Async thread) m) thread
instance UniqueEffect Async IO AsyncThread
-- | The 'IO' implementation uses the @async@ library.
instance MonadEffect (Async AsyncThread) IO where
    effect = AsyncMethods
        (fmap (AsyncThread . fmap return) . Async.async)
        (\(AsyncThread as) -> join (Async.wait as))

-- | This will discard the @'MonadEffect' 'Async' m@ constraint by forcing @m@ to be 'IO'.
--   The functions doesn't actually do anything, the real implementation is given by the
--   @'MonadEffect' 'Async' IO@ instance which uses the @async@ package.
implementAsyncViaIO :: IO a -> IO a
implementAsyncViaIO = id

parallelMapM :: (MonadEffect (Async thread) m, Traversable t) => (a -> m b) -> t a -> m (t b)
parallelMapM f = mapM waitAsync <=< mapM (async . f)

parallelMapM_ :: (MonadEffect (Async thread) m, Traversable t) => (a -> m b) -> t a -> m ()
parallelMapM_ f = mapM_ waitAsync <=< mapM (async . f)
