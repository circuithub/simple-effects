{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs #-}
module Control.Effects.Reader (module Control.Effects.Reader, module Control.Effects) where

import Control.Effects

data ReadEnv e = ReadEnv
data instance Effect (ReadEnv e) method mr where
    ReadEnvMsg :: Effect (ReadEnv e) 'ReadEnv 'Msg
    ReadEnvRes :: { getReadEnvRes :: e } -> Effect (ReadEnv e) 'ReadEnv 'Res

readEnv :: forall e m. MonadEffect (ReadEnv e) m => m e
readEnv = getReadEnvRes <$> effect ReadEnvMsg

handleReadEnv :: Functor m => m e -> EffectHandler (ReadEnv e) m a -> m a
handleReadEnv m = handleEffect (\ReadEnvMsg -> ReadEnvRes <$> m)

handleSubreader :: MonadEffect (ReadEnv e) m => (e -> e') -> EffectHandler (ReadEnv e') m a -> m a
handleSubreader f = handleReadEnv (f <$> readEnv)
