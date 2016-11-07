{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Control.Effects.Reader (module Control.Effects.Reader, module Control.Effects) where

import Interlude

import Control.Lens

import Control.Effects

data ReadEnv e

type instance EffectMsg (ReadEnv e) = ()
type instance EffectRes (ReadEnv e) = e

readEnv :: forall m e. MonadEffect (ReadEnv e) m => m e
readEnv = effect (Proxy :: Proxy (ReadEnv e)) ()

handleReadEnv :: Monad m => m e -> EffectHandler (ReadEnv e) m a -> m a
handleReadEnv = handleEffect . const

handleSubreader :: MonadEffect (ReadEnv e) m => (e -> e') -> EffectHandler (ReadEnv e') m a -> m a
handleSubreader f = handleReadEnv (f <$> readEnv)