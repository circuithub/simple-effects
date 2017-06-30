{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DataKinds, GADTs #-}
-- | The regular old 'MonadReader' effect with some differences. First, there's no functional
--   dependency limiting your stack to a single environment type. This means less type inference so
--   it might not be enough to just write 'readEnv'. Write 'readEnv @MyEnvType' instead using
--   TypeApplications.
--
--   Second, the function has a less generic name and is called 'readEnv'.
--
--   Third, since it's a part of this effect framework, you get a 'handleReadEnv' function with
--   which you can provide a different environment implementation _at runtime_.
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
