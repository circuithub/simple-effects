{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tutorial.Test where

import Data.Text as T
import Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Effects.State
import Control.Effects.List
import Control.Concurrent
import Control.Monad.Runnable
import Control.Monad.Trans
import Control.Monad

addFruit :: (MonadIO m, MonadEffect (State [Text]) m) => m ()
addFruit = do
    liftIO (T.putStrLn "Name a type of fruit please")
    fruit <- liftIO T.getLine
    knownFruits <- getState
    setState (fruit : knownFruits)

main1 :: IO ()
main1 = implementStateViaStateT @[Text] [] $ do
    addFruit
    addFruit
    addFruit
    fruits <- getState @[Text]
    liftIO (print fruits)

main2 :: IO ()
main2 =
    evaluateAll $
    implementStateViaStateT @[Text] [] $ do
        addFruit
        addFruit
        addFruit
        fruits <- getState @[Text]
        fruit <- choose fruits
        liftIO (print fruit)

main3 :: IO ()
main3 = do
    evaluateAll $
        implementStateViaStateT @Int 0 $ do
            setState @Int 1
            choose (Prelude.replicate 3 ())
            setState . succ =<< getState @Int
            liftIO . print =<< getState @Int
    implementStateViaStateT @Int 0 $
        evaluateAll $ do
            setState @Int 1
            choose (Prelude.replicate 3 ())
            setState . succ =<< getState @Int
            liftIO . print =<< getState @Int

main4 :: IO ()
main4 = do
    lst <- evaluateToList $
        implementStateViaStateT @Int 0 $ do
            setState @Int 1
            choose (Prelude.replicate 3 ())
            setState . succ =<< getState @Int
            getState @Int
    print lst
    implementStateViaStateT @Int 0 $ do
        evaluateAll $ do
            setState @Int 1
            choose (Prelude.replicate 3 ())
            setState . succ =<< getState @Int
        liftIO . print =<< getState @Int

data Fork
instance Effect Fork where
    data EffMethods Fork m = ForkMethods
        { _fork :: m () -> m (Maybe ThreadId) }
    type CanLift Fork t = RunnableTrans  t
    mergeContext mm = ForkMethods
        (\a -> do
            ForkMethods m <- mm
            m a)
    liftThrough (ForkMethods f) = ForkMethods
        (\a -> do
            st <- currentTransState
            lift (f (void (runTransformer a st)))
            )

instance MonadEffect Fork IO where
    effect = ForkMethods (fmap Just . forkIO)

fork :: MonadEffect Fork m => m () -> m (Maybe ThreadId)
fork = _fork effect

testMethod :: (MonadEffects '[State Int, Fork] m, MonadIO m) => m ()
testMethod = do
    modifyState @Int (+10)
    tid <- void $ fork $ do
        liftIO . print =<< getState @Int
        liftIO $ threadDelay 2000000
        modifyState @Int (+10)
        liftIO . print =<< getState @Int
    liftIO $ threadDelay 1000000
    liftIO $ print tid
    modifyState @Int (+10)
    liftIO . print =<< getState @Int
    liftIO $ threadDelay 3000000
    liftIO . print =<< getState @Int

main5 :: IO ()
main5 = implementStateViaStateT @Int 0 testMethod