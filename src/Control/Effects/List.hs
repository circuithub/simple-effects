{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts, MultiParamTypeClasses #-}
module Control.Effects.List
    ( module Control.Effects.List
    , module ListT ) where

import Prelude hiding (splitAt, head)
import Import

import ListT hiding (take)

import Control.Effects

data NonDeterministic

type instance EffectMsg1 NonDeterministic = []
type instance EffectRes1 NonDeterministic = Identity
type instance EffectCon1 NonDeterministic a = ()

instance Monad m => MonadEffect1 NonDeterministic (ListT m) where
    effect1 _ = fmap Identity . fromFoldable

-- | Runs the rest of the computation for every value in the list
choose :: MonadEffect1 NonDeterministic m => [a] -> m a
choose = fmap runIdentity . effect1 (Proxy :: Proxy NonDeterministic)

-- | Signals that this branch of execution failed to produce a result.
deadEnd :: MonadEffect1 NonDeterministic m => m a
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

-- | Same as 'foldAllResults' but the folding function has the ability to terminate eary by
--   returning Nothing.
foldWithEarlyTermination :: Monad m => (r -> a -> m (Maybe r)) -> r -> ListT m a -> m r
foldWithEarlyTermination = foldMaybe

-- | Executes only the effects needed to produce the first n results.
evaluateNResults :: Monad m => Int -> ListT m a -> m [a]
evaluateNResults n = fmap fst . splitAt n

-- | Executes only the effects needed to produce a single result.
evaluateOneResult :: Monad m => ListT m a -> m (Maybe a)
evaluateOneResult = head
