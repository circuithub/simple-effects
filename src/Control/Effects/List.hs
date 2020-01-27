{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, GADTs, RankNTypes, NoMonomorphismRestriction, PackageImports #-}
-- | Add non-determinism to your monad. Uses the 'ListT' transformer under the hood.
module Control.Effects.List
    ( module Control.Effects.List
    , module ListT ) where

import Prelude hiding (splitAt, head)
import Import

import "list-t" ListT hiding (take)

import Control.Effects

newtype NonDeterminism m = NonDeterminismMethods
    { _choose :: forall a. [a] -> m a }
instance Effect NonDeterminism where
    emap (Covariant nat) (NonDeterminismMethods c) = NonDeterminismMethods (nat . c)
    mergeContext m = NonDeterminismMethods (\a -> do
        lm <- m
        _choose lm a)

-- | Get a value from the list. The choice of which value to take is non-deterministic
--   in a sense that the rest of the computation will be ran once for each of them.
choose :: forall a m. MonadEffect NonDeterminism m => [a] -> m a
NonDeterminismMethods choose = effect

instance Monad m => MonadEffect NonDeterminism (ListT m) where
    effect = NonDeterminismMethods fromFoldable

-- | Signals that this branch of execution failed to produce a result.
deadEnd :: MonadEffect NonDeterminism m => m a
deadEnd = choose []

-- | Execute all the effects and collect the result in a list.
--   Note that this forces all the results, no matter which elements of the result list you end
--   up actually using. For lazyer behavior use the other handlers.
evaluateToList :: Monad m => ListT m a -> m [a]
evaluateToList = toList

-- | Given a function, apply it to all the results.
traverseAllResults :: Monad m => (a -> m ()) -> ListT m a -> m ()
traverseAllResults = traverse_

-- | Given a folding function, fold over every result. If you want to terminate eary, use the
--   'foldWithEarlyTermination' instead.
foldAllResults :: Monad m => (r -> a -> m r) -> r -> ListT m a -> m r
foldAllResults = fold

-- | Same as 'foldAllResults' but the folding function has the ability to terminate early by
--   returning Nothing.
foldWithEarlyTermination :: Monad m => (r -> a -> m (Maybe r)) -> r -> ListT m a -> m r
foldWithEarlyTermination = foldMaybe

-- | Executes only the effects needed to produce the first n results.
evaluateNResults :: Monad m => Int -> ListT m a -> m [a]
evaluateNResults n = fmap fst . splitAt n

-- | Executes only the effects needed to produce a single result.
evaluateOneResult :: Monad m => ListT m a -> m (Maybe a)
evaluateOneResult = head

-- | Execute all the effects but discard their results.
evaluateAll :: Monad m => ListT m a -> m ()
evaluateAll = void . evaluateToList
