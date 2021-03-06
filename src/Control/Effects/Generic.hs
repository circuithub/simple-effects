{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications, RankNTypes, DataKinds, ViewPatterns #-}
{-# LANGUAGE EmptyCase, FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Control.Effects.Generic where

import qualified GHC.Generics as Gen
import GHC.Generics
import Control.Monad.Trans
import GHC.TypeLits
import Control.Monad (join)


class (Generic (a m), Generic (a (t m)), SimpleMethodsRep (Rep (a m)) (Rep (a (t m))) m t)
    => SimpleMethods a (m :: * -> *) (t :: (* -> *) -> * -> *) where
    liftSimple :: a m -> a (t m)
instance (Generic (a m), Generic (a (t m)), SimpleMethodsRep (Rep (a m)) (Rep (a (t m))) m t)
    => SimpleMethods a m t where
    liftSimple = Gen.to . liftSimpleRep @(Rep (a m)) @(Rep (a (t m))) @m @t . Gen.from

class SimpleMethodsRep r r' (m :: * -> *) (t :: (* -> *) -> * -> *) where
    liftSimpleRep :: r x -> r' x
instance SimpleMethodsRep c c' m t => SimpleMethodsRep (M1 a b c) (M1 a b c') m t where
    liftSimpleRep (M1 x) = M1 (liftSimpleRep @c @c' @m @t x)
instance (SimpleMethodsRep a a' m t, SimpleMethodsRep b b' m t)
    => SimpleMethodsRep (a :*: b) (a' :*: b') m t where
    liftSimpleRep (a :*: b) = liftSimpleRep @a @a' @m @t a :*: liftSimpleRep @b @b' @m @t b
instance (SimpleMethodsRep a a' m t, SimpleMethodsRep b b' m t)
    => SimpleMethodsRep (a :+: b) (a' :+: b') m t where
    liftSimpleRep (L1 a) = L1 (liftSimpleRep @a @a' @m @t a)
    liftSimpleRep (R1 b) = R1 (liftSimpleRep @b @b' @m @t b)
instance SimpleMethodsRep U1 U1 m t where
    liftSimpleRep U1 = U1
instance SimpleMethodsRep V1 V1 m t where
    liftSimpleRep v = case v of {}
instance SimpleMethod a a' m t
    => SimpleMethodsRep (K1 x a) (K1 x a') m t where
    liftSimpleRep (K1 m) = K1 (liftMethod @a @a' @m @t m)

class SimpleMethod f ft (m :: * -> *) (t :: (* -> *) -> * -> *) where
    liftMethod :: f -> ft
instance SimpleMethod f ft m t => SimpleMethod (a -> f) (a -> ft) m t where
    liftMethod f a = liftMethod @f @ft @m @t (f a)
instance {-# OVERLAPPABLE #-}
    ForceError (TypeError
        ('Text "Parameters of methods can't depend on the monadic context."
        ':$$: 'Text "The parameter `" ':<>: 'ShowType a ':<>: 'Text "` depends on `"
        ':<>: 'ShowType m ':<>: 'Text "`"))
    => SimpleMethod (a -> f) (a' -> ft) m t where
    liftMethod = error "Unreachable"
instance (MonadTrans t, Monad m) => SimpleMethod (m a) (t m a) m t where
    liftMethod m = lift m
instance {-# OVERLAPPABLE #-}
    ForceError (TypeError
        ('Text "The result of all methods must be monadic."
        ':$$: 'Text "One of the methods' result is of type `" ':<>: 'ShowType a
        ':<>: 'Text "`. Maybe try `" ':<>: 'ShowType (m a) ':<>: 'Text "` instead."))
    => SimpleMethod a b m t where
    liftMethod = error "Unreachable"

genericLiftThrough ::
    forall t e m. (MonadTrans t, Monad m, Monad (t m), SimpleMethods e m t)
    => e m -> e (t m)
genericLiftThrough = liftSimple
{-# INLINE genericLiftThrough #-}





class (Generic (a m), MonadicMethodsRep (Rep (a m)) m, Monad m) => MonadicMethods a m where
    mergeMonadicMethods :: m (a m) -> a m
instance (Generic (a m), MonadicMethodsRep (Rep (a m)) m, Monad m) => MonadicMethods a m where
    mergeMonadicMethods m = Gen.to (mergeMonadicMethodsRep @(Rep (a m)) @m (fmap Gen.from m))

class MonadicMethodsRep r m where
    mergeMonadicMethodsRep :: m (r x) -> r x
instance (MonadicMethodsRep c m, Functor m) => MonadicMethodsRep (M1 a b c) m where
    mergeMonadicMethodsRep m = M1 (mergeMonadicMethodsRep @c @m (fmap unM1 m))
instance (MonadicMethodsRep a m, MonadicMethodsRep b m, Functor m)
    => MonadicMethodsRep (a :*: b) m where
    mergeMonadicMethodsRep m =
        mergeMonadicMethodsRep @a @m (fmap l m)
        :*: mergeMonadicMethodsRep @b @m (fmap r m)
        where
        l (x :*: _) = x
        r (_ :*: x) = x
instance
    ForceError (TypeError
        ('Text "Can't automatically derive Effect instance for an effect with multiple constructors."))
    => MonadicMethodsRep (a :+: b) m where
    mergeMonadicMethodsRep = error "Unreachable"
instance MonadicMethodsRep U1 m where
    mergeMonadicMethodsRep _ = U1
instance
    ForceError (TypeError
        ('Text "Can't automatically derive Effect instance for an effect with no constructors."))
    => MonadicMethodsRep V1 m where
    mergeMonadicMethodsRep = error "Unreachable"
instance (MonadicMethod a m, Functor m)
    => MonadicMethodsRep (K1 x a) m where
    mergeMonadicMethodsRep m = K1 (mergeMonadicMethod @a @m (fmap unK1 m))

class MonadicMethod a m where
    mergeMonadicMethod :: m a -> a
instance {-# INCOHERENT #-}
    (MonadicMethod b m, Functor m)
    => MonadicMethod (a -> b) m where
    mergeMonadicMethod m a = mergeMonadicMethod (fmap ($ a) m)
instance Monad m => MonadicMethod (m a) m where
    mergeMonadicMethod = join
instance {-# OVERLAPPABLE #-}
    ForceError (TypeError
        ('Text "The result of all methods must be monadic."
        ':$$: 'Text "One of the methods' result is of type `" ':<>: 'ShowType a
        ':<>: 'Text "`. Maybe try `" ':<>: 'ShowType (m a) ':<>: 'Text "` instead."))
    => MonadicMethod a m where
    mergeMonadicMethod = error "Unreachable"

genericMergeContext :: MonadicMethods a m => m (a m) -> a m
genericMergeContext = mergeMonadicMethods
{-# INLINE genericMergeContext #-}




class (Generic (a m), Generic (a n), MapSimpleMethodsRep (Rep (a m)) (Rep (a n)) m n)
    => GenericEmap a (m :: * -> *) (n :: * -> *) where
    emapSimple :: (forall x. m x -> n x) -> a m -> a n
instance (Generic (a m), Generic (a n), MapSimpleMethodsRep (Rep (a m)) (Rep (a n)) m n)
    => GenericEmap a m n where
    emapSimple f = Gen.to . emapSimpleRep @(Rep (a m)) @(Rep (a n)) @m @n f . Gen.from

class MapSimpleMethodsRep r r' (m :: * -> *) (n :: * -> *) where
    emapSimpleRep :: (forall y. m y -> n y) -> r x -> r' x
instance MapSimpleMethodsRep c c' m n => MapSimpleMethodsRep (M1 a b c) (M1 a b c') m n where
    emapSimpleRep f (M1 x) = M1 (emapSimpleRep @c @c' @m @n f x)
instance (MapSimpleMethodsRep a a' m n, MapSimpleMethodsRep b b' m n)
    => MapSimpleMethodsRep (a :*: b) (a' :*: b') m n where
    emapSimpleRep f (a :*: b) = emapSimpleRep @a @a' @m @n f a :*: emapSimpleRep @b @b' @m @n f b
instance (MapSimpleMethodsRep a a' m n, MapSimpleMethodsRep b b' m n)
    => MapSimpleMethodsRep (a :+: b) (a' :+: b') m n where
    emapSimpleRep f (L1 a) = L1 (emapSimpleRep @a @a' @m @n f a)
    emapSimpleRep f (R1 b) = R1 (emapSimpleRep @b @b' @m @n f b)
instance MapSimpleMethodsRep U1 U1 m n where
    emapSimpleRep _ U1 = U1
instance MapSimpleMethodsRep V1 V1 m n where
    emapSimpleRep _ v = case v of {}
instance MapSimpleMethod a a' m n
    => MapSimpleMethodsRep (K1 x a) (K1 x a') m n where
    emapSimpleRep f (K1 m) = K1 (emapSimpleMethod @a @a' @m @n f m)

class MapSimpleMethod f ft (m :: * -> *) (n :: * -> *) where
    emapSimpleMethod :: (forall x. m x -> n x) -> f -> ft
instance MapSimpleMethod f ft m n => MapSimpleMethod (a -> f) (a -> ft) m n where
    emapSimpleMethod nat f a = emapSimpleMethod @f @ft @m @n nat (f a)
instance {-# OVERLAPPABLE #-}
    ForceError (TypeError
        ('Text "Parameters of methods can't depend on the monadic context."
        ':$$: 'Text "The parameter `" ':<>: 'ShowType a ':<>: 'Text "` depends on `"
        ':<>: 'ShowType m ':<>: 'Text "`"))
    => MapSimpleMethod (a -> f) (a' -> ft) m n where
    emapSimpleMethod _ = error "Unreachable"
instance {-# INCOHERENT #-} MapSimpleMethod (m a) (n a) m n where
    emapSimpleMethod nat m = nat m
instance {-# OVERLAPPABLE #-}
    ForceError (TypeError
        ('Text "The result of all methods must be monadic."
        ':$$: 'Text "One of the methods' result is of type `" ':<>: 'ShowType a
        ':<>: 'Text "`. Maybe try `" ':<>: 'ShowType (m a) ':<>: 'Text "` instead."))
    => MapSimpleMethod a b m n where
    emapSimpleMethod _ = error "Unreachable"

genericEmap ::
    forall n e m. (Generic (e m), GenericEmap e m n)
    => (forall x. m x -> n x) -> e m -> e n
genericEmap = emapSimple
{-# INLINE genericEmap #-}





class ForceError (x :: *)
