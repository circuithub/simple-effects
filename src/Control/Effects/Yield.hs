{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-| The @'Yield' a@ effect lets a computation produce values of type @a@ during it's execution. -}
module Control.Effects.Yield where

import Import
import Control.Effects
import Control.Effects.Signal
import Control.Effects.List
import Control.Effects.Async
import GHC.Generics
import Control.Concurrent.MVar
import Control.Concurrent.Chan

data Yield a

instance Effect (Yield a) where
    data EffMethods (Yield a) m = YieldMethods
        { _yield :: a -> m () }
        deriving (Generic)

-- | Output a value of type @a@. The semantics are determined by the implementation, but usually this
--   will block until the next value is requested by the consumer.
yield :: forall a m. MonadEffect (Yield a) m => a -> m ()
YieldMethods yield = effect

-- | Implement 'Yield' by using non-determinism to output each of the values. This means you can
--   use the functions from "Control.Effects.List" to choose how to consume them. For example,
--   using 'evaluateToList' will give you a list of all yielded values. It also means the 'yield'
--   calls won't block since all the values are requested. Other consumer functions give you more
--   control.
implementYieldViaNonDeterminism ::
    forall a m b. MonadEffect NonDeterminism m
    => RuntimeImplemented (Yield a) (RuntimeImplemented (Signal a ()) (ExceptT a m)) b
    -> m a
implementYieldViaNonDeterminism m = m
    & (>> deadEnd)
    & implement (YieldMethods (signal @a))
    & handleSignal @a (\a -> do
        b <- choose [True, False]
        return $ if b then Break a else Resume ())

-- | Implement 'Yield' through an 'MVar'. The result is a monadic action (the inner one) that
--   returns one yielded
--   value or 'Nothing' if the computation is finished. All subsequent calls will also return
--   'Nothing'. Each execution of this action continues execution in the provided computation,
--   which is otherwise suspended.
--
--   If the provided computation forks new threads and doesn't wait for them to finish, 'Nothing'
--   may be returned prematurely (in the sense that maybe there's still a live thread yielding
--   values).
--
--   Since the yielding is done through a shared 'MVar', this
--   implementation is suitable to be run with multiple threads. Scheduling which thread gets
--   continued is defined by the semantics of 'MVar's.
--
--   [Note]
--      'yield' will block in this implementation.
implementYieldViaMVar ::
    forall a m b. (MonadIO m, MonadEffect Async m)
    => RuntimeImplemented (Yield a) m b -> m (m (Maybe a))
implementYieldViaMVar m = do
    mv <- liftIO newEmptyMVar
    block <- liftIO newEmptyMVar
    done <- liftIO $ newMVar False
    void $ async $ do
        liftIO $ takeMVar block
        void $ m & implement (YieldMethods (\a -> liftIO $ do
            putMVar mv (Just a)
            takeMVar block ))
        liftIO $ do
            void $ swapMVar done True
            void $ tryPutMVar mv Nothing
    return $ liftIO $ do
        d <- readMVar done
        if d then return Nothing
        else do
            putMVar block ()
            takeMVar mv


-- | Implements 'Yield' through a 'Chan'. The resulting monadic action (the inner one) reads one
--   value from the queue. 'Nothing' means the provided computation is done. If the provided
--   computation forks new threads and doesn't wait for them to finish, 'Nothing' may be written
--   prematurely (in the sense that maybe there's still a live thread yielding values).
--
--  [Note]
--      'yield' will /not/ block in this implementation.
implementYieldViaChan ::
    forall a m b. (MonadIO m, MonadEffect Async m)
    => RuntimeImplemented (Yield a) m b -> m (m (Maybe a))
implementYieldViaChan m = do
    ch <- liftIO newChan
    done <- liftIO $ newMVar False
    void $ async $ do
        void $ m & implement (YieldMethods (liftIO . writeChan ch . Just))
        liftIO $ writeChan ch Nothing
    return $ liftIO $ do
        d <- readMVar done
        if d then return Nothing
        else readChan ch >>= \case
            Nothing -> do
                void $ swapMVar done True
                return Nothing
            Just a -> return (Just a)

-- | A convenience function to go through all the yielded results. Use in combination with one
--   of the implementations. Collects a list of values.
traverseYielded :: Monad m => m (Maybe a) -> (a -> m b) -> m [b]
traverseYielded m f = m >>= \case
    Nothing -> return []
    Just a -> do
        b <- f a
        bs <- traverseYielded m f
        return (b : bs)

-- | A convenience function to go through all the yielded results. Use in combination with one
--   of the implementations. Discards the computed values.
traverseYielded_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
traverseYielded_ m f = m >>= \case
    Nothing -> return ()
    Just a -> f a >> traverseYielded_ m f