{-# LANGUAGE FlexibleContexts, UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fplugin=Control.Effects.Plugin #-}
module Fundep where

import Control.Effects.State
import Control.Monad.IO.Class

testPlugin :: (MonadIO m, MonadEffect (State Int) m, Show Int) => m ()
testPlugin = do
    s <- getState
    liftIO $ print s
