{-# LANGUAGE ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}
module Control.Effects.List
    ( module Control.Effects.List
    , module ListT ) where

import Interlude hiding (toList, traverse_, fold, foldMaybe, splitAt, head)

import ListT hiding (take)

import Control.Effects

data List

type instance EffectMsg1 List = []
type instance EffectRes1 List = Identity
type instance EffectCon1 List a = ()

-- | Runs the rest of the computation for every value in the list
choose :: MonadEffect1 List m => [a] -> m a
choose = fmap runIdentity . effect1 (Proxy :: Proxy List)

-- | Signals that this branch of execution failed to produce a result.
deadEnd :: MonadEffect1 List m => m a
deadEnd = choose []

-- | A generic handler for the List effect. Takes a handler for the ListT transformer as a param.
handleList :: Monad m => (ListT m a -> m b) -> EffectHandler1 List (ListT m) a -> m b
handleList f = f . handleEffect1 (fmap Identity . fromFoldable)

-- | Execute all the effects and collect the result in a list.
--   Note that this forces all the results, no matter which elements of the result list you end
--   up actually using. For lazyer behavior use the other handlers.
evaluateToList :: Monad m => EffectHandler1 List (ListT m) a -> m [a]
evaluateToList = handleList toList

-- | Given a function, apply it to all the results.
traverseAllResults :: Monad m => (a -> m ()) -> EffectHandler1 List (ListT m) a -> m ()
traverseAllResults handler = handleList (traverse_ handler)

-- | Given a folding function, fold over every result. If you want to terminate eary, use the
--   `foldWithEarlyTermination` instead.
foldAllResults :: Monad m => (r -> a -> m r) -> r -> EffectHandler1 List (ListT m) a -> m r
foldAllResults f i = handleList (fold f i)

-- | Same as `foldAllResults` but the folding function has the ability to terminate eary by
--   returning Nothing.
foldWithEarlyTermination :: Monad m => (r -> a -> m (Maybe r)) -> r
                                    -> EffectHandler1 List (ListT m) a -> m r
foldWithEarlyTermination f i = handleList (foldMaybe f i)

-- | Executes only the effects needed to produce the first n results.
evaluateNResults :: Monad m => Int -> EffectHandler1 List (ListT m) a -> m [a]
evaluateNResults n = handleList (fmap fst . splitAt n)

-- | Executes only the effects needed to produce a single result.
evaluateOneResult :: Monad m => EffectHandler1 List (ListT m) a -> m (Maybe a)
evaluateOneResult = handleList head

test :: IO (Maybe Int)
test = evaluateOneResult (return 1)
