{-# LANGUAGE FlexibleContexts #-}
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
    where effectsSimple' = setState . (+ (1 :: Int)) =<< getState

main :: IO ()
main =
    defaultMain
        [ bench "MTL state" $ nfIO (execStateT mtlSimple 0)
        , bench "Effects state" $ nfIO (handleStateIO (0 :: Int) (effectsSimple >> getState) :: IO Int)
        ]
