{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Control.Effects.Parallel where

import Import

import GHC.MVar
import GHC.IO.Unsafe
import Data.Array.IO
import Control.Concurrent

import Control.Monad.Runnable
import Control.Effects.State

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    h <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar h ())
    return h

appendState :: forall s m a proxy. (Semigroup s, MonadEffectState s m)
            => proxy s -> m a -> m a
appendState _ m = do
    s :: s <- getState
    a <- m
    s' :: s <- getState
    setState (s <> s')
    return a

parallelWithRestore :: forall m a. Runnable m => (m a -> m a) -> [m a] -> m [a]
parallelWithRestore combine tasks = do
    ress <- parallel tasks
    mapM (combine . restoreMonadicState) ress

parallelWithSequence :: Runnable m => [m a] -> m [a]
parallelWithSequence = mapM restoreMonadicState <=< parallel

parallel :: forall m a. Runnable m => [m a] -> m [MonadicResult m a]
parallel tasks = do
    st <- currentMonadicState
    let ress = unsafePerformIO $ do
            arr :: IOArray Int (MonadicResult m a) <- newArray_ (0, n - 1)
            threads <- forM (zip [0..] tasks) $ \(i, t) -> forkThread $ do
                res <- runMonad st t
                writeArray arr i res
            mapM_ takeMVar threads
            getElems arr
    ress `seq` return ress
    where n = length tasks
