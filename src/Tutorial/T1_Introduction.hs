{-# OPTIONS_GHC -Wno-unused-imports #-}
{-| This part of the tutorial will explain the basics behind @simple-effects@ and how to use the
    effects provided by the library. To learn how to implement your own effects, check out the other
    parts.

    You'll need to enable some extensions to follow along: @TypeApplications@, @FlexibleContexts@,
    @OverloadedStrings@, @DataKinds@.
-}
module Tutorial.T1_Introduction (
    -- * State
    -- $state  

    -- * Non-determinism
    -- $nondeterminism

    -- * Order
    -- $order 
    ) where

import Data.Text as T
import Data.Text.IO as T
import Control.Monad.IO.Class
import Control.Effects.State
import Control.Effects.List
{- $state
    Let's say we're writing a function that asks the user to name a fruit and adds their answer to
    a list of already known fruits. Here's what we want to do (in pseudocode)
        
@
addFruit = do
    fruit <- ask "Name a type of fruit please"
    knownFruits <- getCurrentlyKnownFruits
    setCurrentlyKnownFruits (fruit : knownFruits)
@

    Our function needs to get input from the user, so we'll need a way to do IO. It also modifies
    a piece of state: a list of fruits. We state those requirements in the signature.

@
addFruit :: ('MonadIO' m, 'MonadEffect' ('State' ['Text']) m) => m ()
@

    The 'MonadIO' constraint comes from "Control.Monad.IO.Class" of the @transformers@ package and
    it lets us do IO without forcing our monad to be 'IO'.

    The other constraint, 'MonadEffect', is how you specify effects using this library. It says that
    to run @addFruit@ you need to provide an implementation for a state holding a list of 'Text's.

    Here's how we might define the @addFruit@ function.

@
addFruit :: ('MonadIO' m, 'MonadEffect' ('State' ['Text']) m) => m ()
addFruit = do
    'liftIO' ('T.putStrLn' "Name a type of fruit please")
    fruit <- 'liftIO' 'T.getLine'
    knownFruits <- 'getState'
    'setState' (fruit : knownFruits)
@

    [Note] 
        It's possible to have more than one state available to use. Since 'getState' and 'setState'
        need to work with all of them you'll sometimes need to specify the type of state you want to get/set.
        In the above case we didn't need to since the compiler can infer that our state is @['Text']@. 
        It infers @fruit :: 'Text'@ from the signature of 'T.getLine' and it can infer the list part
        since we're using the list cons operator.

        To help the type checker in cases where it can't infer the types, it's convenient to use the
        @TypeApplications@ extension. With it, we can write @fruit <- 'getState' \@['Text']@ and 
        @'setState' \@['Text'] (fruit : knownFruits)@ to be explicit about which state type we mean.


    So lets use our function to ask for three types of fruit. After than we want to print the list.

@
-- this doesn't work yet
main :: 'IO' ()
main = do
    addFruit
    addFruit
    addFruit
    fruits <- 'getState' \@['Text']
    'liftIO' ('print' fruits)
@

    This almost works but we still need to provide an implementation of the state. The simplest way
    to do that is the 'implementStateViaStateT' function. It takes an initial value, and a computation
    that has a state requirement, and it satisfies that requirement. In this case, the initial
    value will be an empty list, and the computation will be our whole do block. 

@
main :: 'IO' ()
main = 'implementStateViaStateT' \@['Text'] [] $ do
    addFruit
    addFruit
    addFruit
    fruits <- 'getState' \@['Text']
    'liftIO' ('print' fruits)
@

    This should work. The reason we don't need to to anything about the 'MonadIO' constraint is
    because the fact that the final result is in the 'IO' monad automatically satisfies it.

    Another thing that we can do with the 'State' effect (and all the other ones provided by the
    @simple-effects@ library) is provide a custom implementation that can depend on runtime values.

    Lets imagine that we have a database and two functions:

@    
getFruits :: 'MonadIO' m => Connection -> m ['Text']
setFruits :: 'MonadIO' m => Connection -> ['Text'] -> m ()
@

    These should get a list of fruits from the database, and store a new list back into it. We can
    use the exact same code as above, just changing the part that implements the state:

@
main :: 'IO' ()
main = do
    conn <- connectToDb "my-connection-string"
    'implement' ('StateMethods' (getFruits conn) (setFruits conn)) $ do
        addFruit
        addFruit
        addFruit
        fruits <- 'getState' \@['Text']
        'liftIO' ('print' fruits)
@

    And now suddenly our fruit list persists between sessions. We can do the same but instead talk
    to some remote API, or read/write from a file, or use a shared variable and run multiple
    computations at the same time...

-}

{- $nondeterminism
    Now lets add an additional effect into the mix. For example, we an use the 'NonDeterminism' effect.

@
main :: 'IO' ()
main = 
    'evaluateAll' $
    'implementStateViaStateT' \@['Text'] [] $ do
        addFruit
        addFruit
        addFruit
        fruits <- 'getState' \@['Text']
        fruit <- 'choose' fruits
        'liftIO' ('print' fruit)
@

    The 'choose' function non-deterministically picks one fruit from the list and prints it. When
    we use it, our @do@ block gets a new constraint. It's type is now 
    
@
('MonadIO' m, 'MonadEffect' ('State' ['Text']) m, 'MonadEffect' 'NonDeterminism' m) => m ()
@

    Therefore we need to provide an implementation of the 'NonDeterminism' effect before we run the
    whole thing. This is what the 'evaluateAll' function does. It runs the computation for each
    non-deterministic possibility, meaning all the fruit will get printed.

    [Note] Instead of repeating 'MonadEffect' for each effect, you can use the 'MonadEffects'
    type family and give it a list of effects instead. Like this 
    @('MonadIO' m, 'MonadEffects' \'['State' ['Text'], 'NonDeterminism'] m) => m ()@

-}

{- $order
    One thing to note is that the order in which you implement effects sometimes matters. 
    For example, handling state first and then non-determinism after will result in the state
    being forked on each non-deterministic branch. Doing it in the reverse order will make the
    state shared between branches meaning that changes in one branch will affect the state when the
    next branch is taken. Here's an example

@
main :: IO ()
main = do
    'evaluateAll' $
        'implementStateViaStateT' \@Int 0 $ do
            'setState' \@Int 1
            'choose' ('Prelude.replicate' 3 ())
            'setState' . 'succ' =<< 'getState' \@Int
            'liftIO' . 'print' =<< 'getState' \@Int
    'Prelude.putStrLn' ""
    'implementStateViaStateT' \@Int 0 $
        'evaluateAll' $ do
            'setState' \@Int 1
            'choose' ('Prelude.replicate' 3 ())
            'setState' . 'succ' =<< 'getState' \@Int
            'liftIO' . 'print' =<< 'getState' \@Int
@
    The first run prints @2 2 2@ because the state is handled first, while the second run prints 
    @2 3 4@. A useful way to get an intuitive understanding of which order does what is to consider
    that in the second case, after we handle non-determinism we can still get the state value because
    we have yet to handle that effect. But if state was forked on each choice there could be many
    possible state values after the whole computation finishes. Which one would 'getState' return?
    The only way it makes sense is if the state is shared since it keeps things unambiguous.

    On the other hand maybe we do want to see all the possible end states. The following example
    demonstrates how the two orderings let us do those two things.

@
main4 :: IO ()
main4 = do
    lst <- 'evaluateToList' $
        'implementStateViaStateT' \@Int 0 $ do
            'setState' \@Int 1
            'choose' ('Prelude.replicate' 3 ())
            'setState' . 'succ' =<< 'getState' \@Int
            'getState' \@Int
    'print' lst
    'implementStateViaStateT' \@Int 0 $ do
        'evaluateAll' $ do
            'setState' \@Int 1
            'choose' ('Prelude.replicate' 3 ())
            'setState' . 'succ' =<< 'getState' \@Int
        'liftIO' . 'print' =<< 'getState' \@Int
@

    The first run gets the state value at the end. This will be the end state for each fork. This
    value becomes the result of the whole 'implementStateViaStateT' block but since we still need
    to handle the non-determinism, this whole block is also ran once for each possible branch.
    Finally, we collect all the results in a list using the 'evaluateToList' function. This handles
    the non-determinism.

    In the second run we get the state /after/ handling non-determinism. This gives us the final
    value of the state shared between all the branches.

    This concludes the first part of the tutorial. To see how other effects are used, check out the
    examples and the documentation of their respective modules.
    
    If you want a more in-depth look into the inner workings of this library, continue to the second
    part of the tutorial: "Tutorial.T2_Details". We'll look at implementing custom effects in part 3,
    "Tutorial.T3_CustomEffects".
-}