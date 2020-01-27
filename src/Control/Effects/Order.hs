{-# LANGUAGE RankNTypes, ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Control.Effects.Order where

import Control.Monad.Trans
import Control.Monad.Runnable
import Data.Kind
import Data.Functor.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Writer

data Phantom (m :: Type -> Type) (n :: Type -> Type) = (Monad m, Monad n) => Phantom

class FromPhantom t where
    fromPhantom :: (Monad m, Monad n) => Phantom m n -> t m n
instance FromPhantom Phantom where
    fromPhantom = id




data Covariant m n = (Monad m, Monad n) => Covariant
    { natural :: forall a. m a -> n a }

class FromCovariant t where
    fromCovariant :: (Monad m, Monad n) => Covariant m n -> t m n
instance FromCovariant Covariant where
    fromCovariant = id
instance FromCovariant Phantom where
    fromCovariant _ = Phantom

liftCovariant :: (Monad m, MonadTrans t, Monad (t m)) => Covariant m (t m)
liftCovariant = Covariant lift




data Invariant m n = forall s r. (Monad m, Monad n) => Invariant
    { mToN :: forall a. m a -> n a
    , currentState :: n s
    , restoreState :: forall a. r a -> n a
    , run :: forall a. n a -> s -> m (r a) }

class FromInvariant t where
    fromInvariant :: (Monad m, Monad n) => Invariant m n -> t m n
instance FromInvariant Invariant where
    fromInvariant = id
instance FromInvariant Covariant where
    fromInvariant (Invariant{ mToN }) = Covariant mToN
instance FromInvariant Phantom where
    fromInvariant _ = Phantom

newtype TransResult t a = TransResult { getTransformerResult :: TransformerResult t a }

runInvariant :: forall t m. (RunnableTrans t, Monad m, Monad (t m)) => Invariant m (t m)
runInvariant = Invariant
    { mToN = lift
    , currentState = currentTransState
    , restoreState = restoreTransState . getTransformerResult @t
    , run = \t s -> TransResult <$> runTransformer t s }




data Isomorphic m n = (Monad m, Monad n) => Isomorphic
    { mToN :: forall a. m a -> n a
    , nToM :: forall a. n a -> m a }

class FromIsomorphic t where
    fromIsomorphic :: (Monad m, Monad n) => Isomorphic m n -> t m n
instance FromIsomorphic Isomorphic where
    fromIsomorphic = id
instance FromIsomorphic Invariant where
    fromIsomorphic (Isomorphic{ mToN, nToM }) = Invariant
        { mToN = mToN
        , currentState = return ()
        , restoreState = return . runIdentity
        , run = \n _ -> Identity <$> nToM n }
instance FromIsomorphic Covariant where
    fromIsomorphic (Isomorphic{ mToN }) = Covariant mToN
instance FromIsomorphic Phantom where
    fromIsomorphic _ = Phantom

-- | unlift . return = return
--   unlift (m >>= f) = unlift m >>= (unlift . f)
class MonadTrans t => MonadTransUnlift t where
    unlift :: Monad m => t m a -> m a
instance MonadTransUnlift IdentityT where
    unlift = runIdentityT
instance Monoid w => MonadTransUnlift (WriterT w) where
    unlift = fmap fst . runWriterT




class (Monad m, Monad (t m)) => LiftableTransformer (f :: (Type -> Type) -> (Type -> Type) -> Type) t m where
    transformation :: f m (t m)
instance (Monad m, Monad (t m)) => LiftableTransformer Phantom t m where
    transformation = Phantom
instance (MonadTrans t, Monad m, Monad (t m)) => LiftableTransformer Covariant t m where
    transformation = liftCovariant
instance (RunnableTrans t, Monad m, Monad (t m)) => LiftableTransformer Invariant t m where
    transformation = runInvariant
instance (Monad m, Monad (t m), MonadTransUnlift t) => LiftableTransformer Isomorphic t m where
    transformation = Isomorphic lift unlift