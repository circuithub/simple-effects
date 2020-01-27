{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-| The 'Async' effect allows you to fork new threads in monads other than just 'IO'.
-}
module Control.Effects.Async where

import Import
import Control.Effects
import qualified Control.Concurrent.Async as Async
import Control.Monad.Runnable
import Data.Maybe

data Async thread m = AsyncMethods
    { _async :: forall a. m a -> m (thread m a)
    , _waitAsync :: forall a. thread m a -> m a
    , _isAsyncDone :: forall a n. thread n a -> m Bool
    , _cancelAsync :: forall a n. thread n a -> m () }

class ThreadIdentifier thread where
    mapThread :: (m a -> n b) -> thread m a -> thread n b

instance ThreadIdentifier thread => Effect (Async thread) where
    type CanLift (Async thread) t = RunnableTrans t
    type Transformation (Async thread) = Invariant
    mergeContext mm = AsyncMethods
        (\a -> mm >>= ($ a) . _async)
        (\a -> mm >>= ($ a) . _waitAsync)
        (\a -> mm >>= ($ a) . _isAsyncDone)
        (\a -> mm >>= ($ a) . _cancelAsync)
    emap Invariant{..} (AsyncMethods f g h i) = AsyncMethods
        (\tma -> do
            st <- currentState
            !res <- mToN (f (run tma st))
            return $ mapThread (mToN >=> restoreState) res
            )
        (\a -> do
            st <- currentState
            res <- mToN (g (mapThread (`run` st) a))
            restoreState res
            )
        (mToN . h)
        (mToN . i)
    liftThrough (AsyncMethods f g h i) = AsyncMethods
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
        (lift . h)
        (lift . i)

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

-- | Check if the asynchronous computation has finished (either normally, or with an exception)
isAsyncDone :: MonadEffect (Async thread) m => thread n a -> m Bool

-- | Abort the asynchronous exception
cancelAsync :: MonadEffect (Async thread) m => thread n a -> m ()
AsyncMethods async waitAsync isAsyncDone cancelAsync = effect

-- | The type that represents the forked computation in the monad @m@ that eventually computes
--   a value of type @a@. Depending on the monad, the computation may produce zero, one or even
--   multiple values of that type.
newtype AsyncThread m a = AsyncThread (Async.Async (m a))
    deriving (Functor, Eq, Ord)
instance ThreadIdentifier AsyncThread where
    mapThread f (AsyncThread as) = AsyncThread (fmap f as)

-- | The 'IO' implementation uses the @async@ library.
instance thread ~ AsyncThread => MonadEffect (Async thread) IO where
    effect = AsyncMethods
        (fmap (AsyncThread . fmap return) . Async.async)
        (\(AsyncThread as) -> join (Async.wait as))
        (\(AsyncThread as) -> isJust <$> Async.poll as)
        (\(AsyncThread as) -> Async.cancel as)

-- | This will discard the @'MonadEffect' 'Async' m@ constraint by forcing @m@ to be 'IO'.
--   The functions doesn't actually do anything, the real implementation is given by the
--   @'MonadEffect' 'Async' IO@ instance which uses the @async@ package.
implementAsyncViaIO :: IO a -> IO a
implementAsyncViaIO = id

-- | Like 'mapM' but the supplied function is run in parallel asynchronously on all the elements.
--   The results will be in the same order as the inputs.
parallelMapM :: forall thread m t a b.
    (MonadEffect (Async thread) m, Traversable t) => (a -> m b) -> t a -> m (t b)
parallelMapM f = mapM (waitAsync @thread) <=< mapM (async . f)

-- | Same as 'parallelMapM_' but discards the result.
parallelMapM_ :: forall thread m t a b.
    (MonadEffect (Async thread) m, Traversable t) => (a -> m b) -> t a -> m ()
parallelMapM_ f = mapM_ (waitAsync @thread) <=< mapM (async . f)
