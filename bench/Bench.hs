{-# LANGUAGE FlexibleContexts, TypeApplications #-}

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Control.Effects.State

import Control.Monad.State.Strict
import Control.Monad.Trans.State.Strict (execStateT)

import Criterion.Main

mtlSimple :: MonadState Int m => m ()
mtlSimple = replicateM_ 1000000 mtlSimple'
    where mtlSimple' = modify' (+ 1)

effectsSimple :: (MonadEffectState Int m) => m ()
effectsSimple = replicateM_ 1000000 effectsSimple'
    where effectsSimple' = modifyState (+ (1 :: Int))

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain
        [ bench "MTL state" $ nfIO (execStateT mtlSimple 0)
        , bench "Effects state" $ nfIO effState
        ]
    where effState = handleStateT (0 :: Int) (effectsSimple >> getState @Int)
