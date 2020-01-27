{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Control.Effects (module Control.Effects, module Control.Effects.Order) where

import Import hiding (liftThrough)
import Control.Monad.Runnable
import Control.Effects.Generic
import GHC.Generics
import Control.Monad.Fail (MonadFail)
import Data.Kind
import Control.Effects.Order

class Effect (e :: (* -> *) -> *) where
    type Transformation e :: (Type -> Type) -> (Type -> Type) -> Type
    type Transformation e = Covariant

    type CanLift e (t :: (* -> *) -> * -> *) :: Constraint
    type CanLift e t = MonadTrans t

    type ExtraConstraint e (m :: * -> *) :: Constraint
    type ExtraConstraint e m = ()

    emap :: forall m n. Transformation e m n -> e m -> e n
    default emap ::
        forall m n. (Generic (e m), GenericEmap e m n, Transformation e ~ Covariant)
        => Transformation e m n -> e m -> e n
    emap (Covariant nat) = genericEmap nat

    liftThrough ::
        forall t m. (CanLift e t, Monad m, Monad (t m))
        => e m -> e (t m)
    default liftThrough ::
        forall t m.
        ( Generic (e m), MonadTrans t, Monad m, Monad (t m)
        , SimpleMethods e m t )
        => e m -> e (t m)
    liftThrough = genericLiftThrough

    mergeContext :: Monad m => m (e m) -> e m
    default mergeContext ::
        (Generic (e m), MonadicMethods e m)
        => m (e m) -> e m
    mergeContext = genericMergeContext

class (Effect e, Monad m, ExtraConstraint e m) => MonadEffect e m where
    effect :: e m
    default effect ::
        ( MonadEffect e m', Monad (t m'), MonadTrans t, t m' ~ m
        , LiftableTransformer (Transformation e) t m' )
        => e m
    effect = emap transformation effect

instance {-# OVERLAPPABLE #-}
    ( MonadEffect e m, Monad (t m), ExtraConstraint e (t m), MonadTrans t
    , LiftableTransformer (Transformation e) t m )
    => MonadEffect e (t m) where
    effect = emap transformation effect

newtype RuntimeImplemented e m a = RuntimeImplemented
    { getRuntimeImplemented :: ReaderT (e m) m a }
    deriving
        ( Functor, Applicative, Monad, MonadPlus, Alternative, MonadState s, MonadIO, MonadCatch
        , MonadThrow, MonadRandom, MonadMask, MonadFail )

instance MonadTrans (RuntimeImplemented e) where
    lift = RuntimeImplemented . lift

instance MonadReader r m => MonadReader r (RuntimeImplemented e m) where
    ask = RuntimeImplemented (lift ask)
    local f (RuntimeImplemented rdr) = RuntimeImplemented (ReaderT (local f . runReaderT rdr))

deriving instance MonadBase b m => MonadBase b (RuntimeImplemented e m)
instance MonadBaseControl b m => MonadBaseControl b (RuntimeImplemented e m) where
    type StM (RuntimeImplemented e m) a = StM (ReaderT (e m) m) a
    liftBaseWith f = RuntimeImplemented $ liftBaseWith $ \q -> f (q . getRuntimeImplemented)
    restoreM = RuntimeImplemented . restoreM

instance RunnableTrans (RuntimeImplemented e) where
    type TransformerResult (RuntimeImplemented e) a = a
    type TransformerState (RuntimeImplemented e) m = e m
    currentTransState = RuntimeImplemented ask
    restoreTransState = return
    runTransformer (RuntimeImplemented m) = runReaderT m

instance (Effect e, Monad m, CanLift e (RuntimeImplemented e), ExtraConstraint e (RuntimeImplemented e m))
    => MonadEffect e (RuntimeImplemented e m) where
    effect = mergeContext $ RuntimeImplemented (liftThrough <$> ask)

implement :: forall e m a. e m -> RuntimeImplemented e m a -> m a
implement em (RuntimeImplemented r) = runReaderT r em

type family MonadEffects effs m :: Constraint where
    MonadEffects '[] m = ()
    MonadEffects (eff ': effs) m = (MonadEffect eff m, MonadEffects effs m)

class UniqueEffect (effName :: k) (m :: * -> *) a | effName m -> a
instance {-# OVERLAPPABLE #-} UniqueEffect effName m a => UniqueEffect effName (t m) a