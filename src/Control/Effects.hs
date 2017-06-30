{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor
           , GeneralizedNewtypeDeriving, UndecidableInstances, StandaloneDeriving
           , IncoherentInstances #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeInType, Rank2Types, TypeOperators, ConstraintKinds #-}
module Control.Effects (module Control.Effects) where

import Import
import Control.Monad.Runnable
import Data.Kind

data MsgOrRes = Msg | Res
data family Effect (effKind :: Type) :: effKind -> MsgOrRes -> Type

class Monad m => MonadEffect effKind m where
    -- | Use the effect described by 'method'.
    effect :: Effect effKind method 'Msg -> m (Effect effKind method 'Res)

newtype EffectWithKind effKind m = EffectWithKind
    { getEffectWithKind :: forall method. Effect effKind method 'Msg -> m (Effect effKind method 'Res) }

-- | The 'EffectHandler' is really just a 'ReaderT' carrying around the function that knows how to
--   handle the effect.
newtype EffectHandler effKind m a = EffectHandler
    { unpackEffectHandler :: ReaderT (EffectWithKind effKind m) m a }
    deriving ( Functor, Applicative, Monad, Alternative, MonadState s, MonadIO, MonadCatch
             , MonadThrow, MonadRandom )

instance MonadTrans (EffectHandler effKind) where
    lift = EffectHandler . lift

instance RunnableTrans (EffectHandler effKind) where
    type TransformerState (EffectHandler effKind) m = EffectWithKind effKind m
    type TransformerResult (EffectHandler effKind) m a = a
    currentTransState = EffectHandler ask
    restoreTransState = return
    runTransformer m = runReaderT (unpackEffectHandler m)

instance MonadReader s m => MonadReader s (EffectHandler effKind m) where
    ask = EffectHandler (lift ask)
    local f (EffectHandler rdr) = EffectHandler (ReaderT $ local f . runReaderT rdr)

deriving instance MonadBase b m => MonadBase b (EffectHandler effKind m)

instance MonadBaseControl b m => MonadBaseControl b (EffectHandler effKind m) where
    type StM (EffectHandler effKind m) a = StM (ReaderT (EffectWithKind effKind m) m) a
    liftBaseWith f = EffectHandler $ liftBaseWith $ \q -> f (q . unpackEffectHandler)
    restoreM = EffectHandler . restoreM

instance {-# OVERLAPPABLE #-} (MonadEffect method m, MonadTrans t, Monad (t m))
         => MonadEffect method (t m) where
    {-# INLINE effect #-}
    effect msg = lift (effect msg)

instance Monad m => MonadEffect effKind (EffectHandler effKind m) where
    {-# INLINE effect #-}
    effect msg = EffectHandler (ReaderT (($ msg) . getEffectWithKind))

-- | Handle the effect described by 'effKind'.
handleEffect ::
    (forall method. Effect effKind method 'Msg -> m (Effect effKind method 'Res))
    -> EffectHandler effKind m a -> m a
handleEffect f eh = runReaderT (unpackEffectHandler eh) (EffectWithKind f)

type family MonadEffects effs m :: Constraint where
    MonadEffects '[] m = ()
    MonadEffects (eff ': effs) m = (MonadEffect eff m, MonadEffects effs m)
