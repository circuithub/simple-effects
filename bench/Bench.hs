{-# LANGUAGE FlexibleContexts, TypeApplications #-}
{-# OPTIONS_GHC -ddump-simpl #-}
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import qualified Control.Effects.State as Eff1
import Control.Monad.State.Strict
import Control.Monad.Trans.State.Strict (execStateT)

import Criterion.Main

modify'' :: MonadState s m => (s -> s) -> m ()
modify'' f = do
    s <- get
    let s' = f s
    s' `seq` put s'
{-# INLINE modify'' #-}

mtlSimple :: MonadState Int m => m ()
mtlSimple = replicateM_ 1000000 mtlSimple'
    where mtlSimple' = modify'' (+ 1)

effectsSimple :: (Eff1.MonadEffect (Eff1.State Int) m) => m ()
effectsSimple = replicateM_ 1000000 effectsSimple'
    where effectsSimple' = Eff1.modifyState (+ (1 :: Int))

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain
        [ bench "MTL state" $ nfIO (execStateT mtlSimple 0)
        , bench "Effects state" $ nfIO effState1
        ]
    where 
    effState1 = Eff1.handleStateT (0 :: Int) (effectsSimple >> Eff1.getState @Int)
