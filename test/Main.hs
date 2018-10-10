{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad
import Control.Effects.Signal
import Control.Effects.State
import Control.Effects.Parallel
import Control.Effects.Early
import Control.Effects.Async
import Control.Effects.List
import Control.Effects.Yield
import Control.Effects.Resource
import Control.Concurrent hiding (yield)
import System.IO

import Data.Function

-- Should infer
ex1 = signal True

-- Should compile
ex2 :: MonadEffect (Throw Bool) m => m ()
ex2 = throwSignal False

ex3 = do
    void $ discardAllExceptions ex1
    void $ showAllExceptions ex2
    handleException (\(_ :: Bool) -> return ()) ex2
    handleSignal (\(_ :: Bool) -> return $ Resume 5) ex1

-- Nested Early
testEarly1 :: Monad m => m Bool
testEarly1 = handleEarly $ do
    return ()
    _ <- earlyReturn True
    _ <- handleEarly $ do
        return ()
        earlyReturn (123 :: Int)
    _ <- testEarly2
    return True

testEarly2 :: Monad m => m Char
testEarly2 = handleEarly $
    earlyReturn 'a'

orderTest :: (MonadEffects '[HandleException Bool, Throw Bool, State Int] m, MonadIO m) => m ()
orderTest = do
    setState (1 :: Int)
    _ :: Either Bool () <- handleToEitherRecursive $ do
        setState (2 :: Int)
        void $ throwSignal True
        setState (3 :: Int)
    st :: Int <- getState
    liftIO (print st)

inc :: Int -> Int
inc !x = x + 1

task :: (MonadEffect (State Int) m) => m Int
task = do
    replicateM_ 10000000 (modifyState inc)
    st <- getState
    st `seq` return st

main :: IO ()
main = do
    orderTest & handleException (\(_ :: Bool) -> return ())
              & implementStateViaStateT (0 :: Int)
    orderTest & implementStateViaStateT (0 :: Int)
              & handleException (\(_ :: Bool) -> return ())
    putStrLn "Starting sequential test"
    replicateM_ 8 (implementStateViaStateT (0 :: Int) task >>= print)
    putStrLn "Sequential test done"
    putStrLn "Starting parallel test"
    implementStateViaStateT (0 :: Int) $ do
        res <- parallelWithSequence (replicate 8 task)
        mapM_ (liftIO . print) res
    putStrLn "Parallel test done"

parallelTest ::
    (MonadEffects '[Async thread, NonDeterminism] m, MonadIO m) => m (thread m (Int, Char))
parallelTest = do
    n <- choose [1,2,3,4]
    async $ do
        liftIO $ threadDelay ((5 - n) * 1000000)
        l <- choose "ab"
        return (n, l)

mainAsync :: IO ()
mainAsync = do
    threads <- evaluateToList parallelTest
    forM_ threads $ \thread ->
        evaluateToList (do
            p <- waitAsync thread
            liftIO $ print p
            )

yieldTest ::
    (MonadEffects '[Yield Int, Async thread] m, MonadIO m) => m ()
yieldTest = do
    yield @Int 5
    t <- async $ do
        liftIO $ putStrLn "yielding 6"
        yield @Int 6
        liftIO $ putStrLn "yielding 10"
        yield @Int 10

    t2 <- async $ do
        liftIO $ putStrLn "yielding 8"
        yield @Int 8
        liftIO $ putStrLn "yielding 9"
        yield @Int 9
    yield @Int 7
    waitAsync t
    waitAsync t2
    return ()

mainYield :: IO ()
mainYield = do
    hSetBuffering stdout LineBuffering
    await <- implementYieldViaMVar @Int yieldTest
    traverseYielded_ await $ \res -> do
        print res
        void getLine

-- testResource = evaluateAll $ bracket
--     (choose [True, False] >>= \tf -> liftIO (putStrLn ("acq " ++ show tf)) >> return tf)
--     (\tf _ -> liftIO $ putStrLn ("cleaning " ++ show tf))
--     (\tf -> if tf then liftIO $ putStrLn "true" else error "io err" >> liftIO (putStrLn "false") )
