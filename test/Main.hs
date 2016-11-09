{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, ScopedTypeVariables #-}
module Main where

import Interlude

import Control.Effects.Signal

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

main :: IO ()
main = return ()
