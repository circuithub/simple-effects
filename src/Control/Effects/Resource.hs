{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeFamilies, MultiParamTypeClasses, LambdaCase #-}
{-# LANGUAGE FlexibleContexts, InstanceSigs, NoMonomorphismRestriction, FlexibleInstances #-}
{-# LANGUAGE DataKinds, UndecidableInstances, TypeOperators, StandaloneDeriving, DeriveAnyClass #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Provides the 'Bracket' effect for handing resource acquisition and safe cleanup.
module Control.Effects.Resource where

import Import hiding (bracket)
import Control.Effects
import Control.Monad.Runnable
import qualified Control.Exception as Ex
import GHC.TypeLits
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.Writer.Strict as SW
import qualified Control.Monad.Trans.Writer.Lazy as LW
import qualified Control.Monad.Trans.RWS.Strict as SR
import qualified Control.Monad.Trans.RWS.Lazy as LR
import ListT

-- | Class of transformers that don't introduce additional exit points to a computation.
--
-- Examples: @'StateT' s@, @'ReaderT' e@, 'IdentityT'
--
-- Counter-examples: @'ExceptT' e@, @'ErrorT' e@, 'MaybeT', 'ListT'
class Unexceptional (t :: (* -> *) -> * -> *)

data Bracket
instance Effect Bracket where
    data EffMethods Bracket m = BracketMethods
        { _bracket ::
            forall resource result cleanupRes.
            m resource -> (resource -> Maybe result -> m cleanupRes) -> (resource -> m result) -> m result }
    type CanLift Bracket t = (RunnableTrans t, Unexceptional t)
    liftThrough :: forall m t. (RunnableTrans t, Monad (t m), Monad m)
        => EffMethods Bracket m -> EffMethods Bracket (t m)
    liftThrough (BracketMethods f) = BracketMethods g
        where
        g :: forall a b c. t m a -> (a -> Maybe c -> t m b) -> (a -> t m c) -> t m c
        g acq cleanup use = do
            st <- currentTransState
            res <- lift (f
                (runTransformer acq st)
                (\tra mtrc -> flip runTransformer st $ do
                    a <- restoreTransState tra
                    c <- case mtrc of
                        Nothing -> return Nothing
                        Just trc -> Just <$> restoreTransState trc
                    cleanup a c)
                (\tra -> flip runTransformer st $ do
                    a <- restoreTransState tra
                    use a))
            restoreTransState res
    mergeContext mm = BracketMethods $ \acq cln use -> do
        BracketMethods f <- mm
        f acq cln use

-- | @'bracket' acq cln use@ acquires the resource by running @acq@.
-- If this computation aborts, the exception won't be handled and no cleanup will be performed since
-- the resource wasn't acquired. Then @use@ is called with the resource. Regardless if @use@ threw
-- an exception/aborted or finished normally, @cln@ is called with the resource and possibly with
-- the result of @use@ (if it didn't abort). If there was an exception, it's rethrown: bracket
-- is not meant to be used for exception handling.
--
-- An exception in this context is anything from actual @IO@ exceptions for pure ones \"thrown\" by
-- 'ExceptT' or 'MaybeT'. In case of 'IO', the resource acquisition and cleanup are masked from
-- async exceptions.
--
-- Since this function can be used on almost any transformer stack, care needs to be taken that
-- all the transformers that /can/ throw exceptions get handled. This is why the effect isn't
-- implicitly lifted through unknown transformers, only though ones that are instances of
-- 'Unexceptional'. If your transformer doesn't introduce new exit points, give it an instance of
-- that class. There are no methods to implement.
bracket :: MonadEffect Bracket m =>
    m resource -> (resource -> Maybe result -> m cleanupRes) -> (resource -> m result) -> m result
BracketMethods bracket = effect

-- | Use bracketing and masking for IO exceptions
instance MonadEffect Bracket IO where
    effect = BracketMethods $ \acq cln use -> Ex.mask $ \unmasked -> do
        resource <- acq
        b <- unmasked (use resource) `Ex.catch` \(e :: SomeException) -> do
            _ <- cln resource Nothing
            throwM e
        _ <- cln resource (Just b)
        return b

-- | Identity can't throw or acquire in a meaningful way
instance MonadEffect Bracket Identity where
    effect = BracketMethods $ \acq _ use -> do
        res <- acq
        use res

-- | Source: http://hackage.haskell.org/package/exceptions-0.10.0/docs/src/Control-Monad-Catch.html#line-674
instance MonadEffect Bracket m => MonadEffect Bracket (ExceptT e m) where
    effect = BracketMethods $ \acq cln use -> do
        eres <- lift $ bracket
            (runExceptT acq)
            (\eres exitCase -> case eres of
                Left e -> return (Left e) -- nothing to release, acquire didn't succeed
                Right res -> case exitCase of
                    Just (Right b) -> runExceptT (cln res (Just b))
                    _ -> runExceptT (cln res Nothing))
            (\case
                Right res -> runExceptT $ use res
                Left e -> return (Left e))
        case eres of
            Left e -> throwE e
            Right res -> return res

instance MonadEffect Bracket m => MonadEffect Bracket (MaybeT m) where
    effect = BracketMethods $ \acq cln use -> do
        eres <- lift $ bracket
            (runMaybeT acq)
            (\mres exitCase -> case mres of
                Nothing -> return Nothing
                Just res -> case exitCase of
                    Just (Just b) -> runMaybeT (cln res (Just b))
                    _ -> runMaybeT (cln res Nothing))
            (\case
                Just res -> runMaybeT $ use res
                Nothing -> return Nothing)
        case eres of
            Nothing -> mzero
            Just res -> return res

-- | Warn about unknown transformers with a type error.
instance {-# OVERLAPPABLE #-} UnexceptionalError t => Unexceptional t
instance Unexceptional (SS.StateT s)
instance Unexceptional (LS.StateT s)
instance Unexceptional (SW.WriterT s)
instance Unexceptional (LW.WriterT s)
instance Unexceptional (SR.RWST r w s)
instance Unexceptional (LR.RWST r w s)
instance Unexceptional IdentityT
instance Unexceptional (ReaderT r)
instance Unexceptional ListT




-- | A simpler version of 'bracket' that doesn't use the results of the parameters.
bracket_ :: MonadEffect Bracket m => m resource -> m cleanupRes -> m result -> m result
bracket_ ack cln use = bracket ack (\_ _ -> cln) (const use)

type family UnexceptionalError (t :: (* -> *) -> * -> *) :: Constraint where
    UnexceptionalError ListT = TypeError
        ( 'Text "ListT is an exceptional transformer since it can produce zero results. The reason \
        \why it isn't handled like ExceptT or MaybeT is because it's unclear what the behavior should \
        \be:"
        ':$$: 'Text "Firstly, it might acquire more than one resource. Is that expected?"
        ':$$: 'Text "More importantly, it may produce more than one result of using a single \
        \resource. How many times should the cleanup function be called then?"
        ':$$: 'Text "Also, should all the resources be acquired at the beginning and released at \
        \the end, or should they be processed one by one?"
        ':$$: 'Text "If you need this instance, please let me know what you think should happen." )
    UnexceptionalError t = TypeError
        ( 'Text "The Bracket effect doesn't know about the transformer " ':<>: 'ShowType t ':$$:
          'Text "While the effect can be used with any transformer that has a RunnableTrans instance, \
          \it's dangerous to do so implicitly because the transformer might introduce an additional \
          \exit point to the computation (like IO, MaybeT, ExceptT and friends do)" ':$$:
          'Text "If you're sure that it doesn't, give it an 'Unexceptional' instance:" ':$$:
          'Text "instance Unexceptional (" ':<>: 'ShowType t ':<>: 'Text ")" )
