{-# LANGUAGE ScopedTypeVariables #-}
module Control.Effects.Parallel where

import Interlude hiding (toList)

import GHC.IO.Unsafe

import Data.Array.IO

import Control.Monad.Runnable

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
    h <- newEmptyMVar
    _ <- forkFinally proc (\_ -> putMVar h ())
    return h

parallelWithRestore :: forall m a. Runnable m
                    => (MonadicResult m a -> MonadicResult m [a] -> MonadicResult m [a])
                    -> [m a]
                    -> m [a]
parallelWithRestore combine tasks = do
    st <- currentMonadicState
    let emptyRes = unsafePerformIO $ runMonad st (return [] :: m [a])
    ress <- parallel tasks
    restoreMonadicState $ foldr combine emptyRes ress

parallel :: forall m a. Runnable m => [m a] -> m [MonadicResult m a]
parallel tasks = do
    st <- currentMonadicState
    return $ unsafePerformIO $ do
        arr :: IOArray Int (MonadicResult m a) <- newArray_ (0, n - 1)
        threads <- forM (zip [0..] tasks) $ \(i, t) -> forkThread $ do
            res <- runMonad st t
            writeArray arr i res
        mapM_ takeMVar threads
        getElems arr
    where n = length tasks
