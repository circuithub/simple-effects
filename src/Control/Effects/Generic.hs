{-# LANGUAGE ScopedTypeVariables, TypeFamilies, PolyKinds, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications, RankNTypes, DataKinds, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Control.Effects.Generic where

import qualified GHC.Generics as Gen
import GHC.Generics
import Control.Monad.Trans
import Data.Proxy
import GHC.TypeLits

data M a

class (Generic (a m), Generic (a (t m)), Generic (a M)) => SimpleMethods a m t where
    liftSimple :: a m -> a (t m)

instance
    ( Rep (a m) ~ D1 m1 (C1 m2 p)
    , Rep (a M) ~ D1 m1 (C1 m2 pM)
    , Rep (a (t m)) ~ D1 m1 (C1 m2 (LiftedProducts p pM m t))
    , ProductOfSimpleMethods p pM m t
    , Generic (a m), Generic (a (t m)), Generic (a M) )
    => SimpleMethods a m t where
    liftSimple a = case Gen.from a of
        M1 (M1 p) -> Gen.to (M1 (M1 (liftProducts (Proxy @m) (Proxy @t) (Proxy @pM) p)))
    {-# INLINE liftSimple #-}

class ProductOfSimpleMethods p pM m t where
    type LiftedProducts p pM m t :: * -> *
    liftProducts :: Proxy m -> Proxy t -> Proxy pM -> p x -> LiftedProducts p pM m t x

instance SimpleMethod f fM m t => ProductOfSimpleMethods (S1 m1 (Rec0 f)) (S1 m1 (Rec0 fM)) m t where
    type LiftedProducts (S1 m1 (Rec0 f)) (S1 m1 (Rec0 fM)) m t =
        (S1 m1 (Rec0 (LiftedMethod f fM m t)))
    liftProducts p1 p2 _ (M1 (K1 f)) = M1 (K1 (liftMethod p1 p2 (Proxy @fM) f))
    {-# INLINE liftProducts #-}
instance
    (ProductOfSimpleMethods f1 f1M m t, ProductOfSimpleMethods f2 f2M m t)
    => ProductOfSimpleMethods (f1 :*: f2) (f1M :*: f2M) m t where
    type LiftedProducts (f1 :*: f2) (f1M :*: f2M) m t =
        LiftedProducts f1 f1M m t :*: LiftedProducts f2 f2M m t
    liftProducts p1 p2 _ (f1 :*: f2) =
        liftProducts p1 p2 (Proxy @f1M) f1 :*: liftProducts p1 p2 (Proxy @f2M) f2
    {-# INLINE liftProducts #-}

class (MonadTrans t, Monad m) => SimpleMethod f fM (m :: * -> *) (t :: (* -> *) -> * -> *) where
    type LiftedMethod f fM m t
    liftMethod :: Proxy m -> Proxy t -> Proxy fM -> f -> LiftedMethod f fM m t
instance (MonadTrans t, Monad m, a ~ m x) => SimpleMethod a (M x) m t where
    type LiftedMethod a (M x) m t = t m x
    liftMethod _ _ _ = lift @t
    {-# INLINE liftMethod #-}
type family FuncRes f where
    FuncRes (a -> b) = b
instance (f ~ (a -> b), SimpleMethod b bM m t, IndependentOfM a m) => SimpleMethod f (a -> bM) m t where
    type LiftedMethod f (a -> bM) m t = a -> LiftedMethod (FuncRes f) bM m t
    liftMethod p1 p2 _ f a = liftMethod p1 p2 (Proxy @bM) (f a :: b)
    {-# INLINE liftMethod #-}
instance {-# OVERLAPPABLE #-}
    ( TypeError ('Text "Effect methods must be monadic actions or functions resulting in monadic actions")
    , Monad m, MonadTrans t )
    => SimpleMethod a b m t

class IndependentOfM (a :: k) (m :: * -> *) where
instance
    (IndependentOfM a m, IndependentOfM b m)
    => IndependentOfM (a b) m
instance
    TypeError
        ('Text "Parameters of methods can't depend on the monadic context ("
        ':<>: 'ShowType m
        ':<>: 'Text ")")
    => IndependentOfM M m
instance {-# OVERLAPPABLE #-}
    IndependentOfM (a :: k) m

genericLiftThrough ::
    forall t e m. (MonadTrans t, Monad m, Monad (t m), SimpleMethods e m t)
    => e m -> e (t m)
genericLiftThrough = liftSimple
{-# INLINE genericLiftThrough #-}



class MonadicMethods a m where
    mergeMonadicMethods :: m (a m) -> a m
instance
    ( Rep (a m) ~ D1 m1 (C1 m2 p)
    , Rep (a M) ~ D1 m1 (C1 m2 pM)
    , ProductOfMonadicMethods p pM a m
    , Generic (a m), Generic (a M) )
    => MonadicMethods a m where
    mergeMonadicMethods a = Gen.to (M1 (M1 (mergeMonadicProducts (Proxy @p) (Proxy @pM) a f)))
        where
        f (Gen.from -> M1 (M1 p)) = p
    {-# INLINE mergeMonadicMethods #-}

class ProductOfMonadicMethods p pM a m where
    mergeMonadicProducts :: Proxy p -> Proxy pM -> m (a m) -> (a m -> p x) -> p x
instance MonadicMethod a f fM m => ProductOfMonadicMethods (S1 m1 (Rec0 f)) (S1 m1 (Rec0 fM)) a m where
    mergeMonadicProducts _ _ ma f = M1 (K1 (mergeMethod (Proxy @fM) (g . f) ma))
        where
        g (M1 (K1 x)) = x
    {-# INLINE mergeMonadicProducts #-}
instance
    (ProductOfMonadicMethods f1 f1M a m, ProductOfMonadicMethods f2 f2M a m)
    => ProductOfMonadicMethods (f1 :*: f2) (f1M :*: f2M) a m where
    mergeMonadicProducts _ _ ma f =
        mergeMonadicProducts (Proxy @f1) (Proxy @f1M) ma (g1 . f)
        :*: mergeMonadicProducts (Proxy @f2) (Proxy @f2M) ma (g2 . f)
        where
        g1 (x :*: _) = x
        g2 (_ :*: x) = x
    {-# INLINE mergeMonadicProducts #-}

class Monad m => MonadicMethod a f fM m where
    mergeMethod :: Proxy fM -> (a m -> f) -> m (a m) -> f
instance (b ~ m x, Monad m) => MonadicMethod a b (M x) m where
    mergeMethod _ f ma = do
        a <- ma
        f a
    {-# INLINE mergeMethod #-}
instance (f ~ (b -> c), Monad m, MonadicMethod a c cM m) => MonadicMethod a f (bM -> cM) m where
    mergeMethod _ f ma b = mergeMethod (Proxy @cM) (g . f) ma
        where
        g = ($ b)
    {-# INLINE mergeMethod #-}
instance {-# OVERLAPPABLE #-}
    ( TypeError ('Text "Effect methods must be monadic actions or functions resulting in monadic actions")
    , Monad m )
    => MonadicMethod a f fM m

genericMergeContext :: MonadicMethods a m => m (a m) -> a m
genericMergeContext = mergeMonadicMethods
{-# INLINE genericMergeContext #-}
