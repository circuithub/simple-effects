{-# LANGUAGE FlexibleContexts, TypeApplications #-}
import Interlude

import GHC.IO.Encoding (setLocaleEncoding, utf16)
import Control.Effects
import Control.Effects.State hiding (mtlSimple, effectsSimple)

import Criterion.Main

mtlSimple :: MonadState Int m => m ()
mtlSimple = replicateM_ 1000000 mtlSimple'
    where mtlSimple' = modify' (+ 1)

effectsSimple :: (MonadEffect (GetState Int) m, MonadEffect (SetState Int) m) => m ()
effectsSimple = replicateM_ 1000000 effectsSimple'
    where effectsSimple' = modifyState (+ (1 :: Int))

main :: IO ()
main =
    defaultMain
        [ bench "MTL state" $ nfIO (execStateT mtlSimple 0)
        , bench "Effects state" $ nfIO effState
        ]
    where effState = handleStateIO @_ @Int @Int 0 (effectsSimple >> getState)
