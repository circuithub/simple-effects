{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Tutorial.Test where

import Data.Text as T
import Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Effects.State
import Control.Effects.List

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