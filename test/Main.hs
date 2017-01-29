{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables, BangPatterns #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad
import Control.Effects.Signal
import Control.Effects.State
import Control.Effects.Parallel
import Control.Effects.Early
import Data.Function

-- Should infer
ex1 = signal True

-- Should compile
ex2 :: Throws Bool m => m ()
ex2 = throwSignal False

ex3 = do
    void $ discardAllExceptions ex1
    void $ showAllExceptions ex2
    handleException (\(_ :: Bool) -> return ()) ex2
    handleSignal (\(_ :: Bool) -> Resume 5) ex1

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

orderTest :: (Handles Bool m, MonadEffectState Int m, MonadIO m) => m ()
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

task :: (MonadEffectState Int m) => m Int
task = do
    replicateM_ 10000000 (modifyState inc)
    st <- getState
    st `seq` return st

main :: IO ()
main = do
    orderTest & handleException (\(_ :: Bool) -> return ())
              & handleStateT (0 :: Int)
    orderTest & handleStateT (0 :: Int)
              & handleException (\(_ :: Bool) -> return ())
    putStrLn "Starting sequential test"
    replicateM_ 8 (handleStateT (0 :: Int) task >>= print)
    putStrLn "Sequential test done"
    putStrLn "Starting parallel test"
    handleStateT (0 :: Int) $ do
        res <- parallelWithSequence (replicate 8 task)
        mapM_ (liftIO . print) res
    putStrLn "Parallel test done"
