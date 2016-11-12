{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Interlude

import Control.Effects.Signal
import Control.Effects.State

-- Should infer
ex1 = signal True

-- Should compile
ex2 :: Throws Bool m => m ()
ex2 = throwSignal False

ex3 = do
    discardAllExceptions ex1
    showAllExceptions ex2
    handleException (\(b :: Bool) -> return ()) ex2
    handleSignal (\(_ :: Bool) -> Resume 5) ex1

orderTest :: (Test Bool m, MonadEffectState Int m, MonadIO m) => m ()
orderTest = do
    setState (1 :: Int)
    _ :: Either Bool () <- handleE $ do
        setState (2 :: Int)
        void $ throwSignal True
        setState (3 :: Int)
    st :: Int <- getState
    print st

main :: IO ()
main = do
    orderTest & handleException (\(_ :: Bool) -> return ())
              & handleStateT (0 :: Int)
    orderTest & handleStateT (0 :: Int)
              & handleException (\(_ :: Bool) -> return ())
