{-# OPTIONS_GHC -Wno-unused-imports #-}
{-| In this part, we'll take a more detailed look at this library. You don't need to know these
    things to use the effects. For the most part, you can also implement your own without reading
    this part. To learn about that check out the next part: "Tutorial.T3_CustomEffects".

    That being said, the details will help you understand potential compiler errors you might get.
    Also, implementing more complex effects does require a bit of an understanding of the internals.
-}

module Tutorial.T2_Details (
    -- * Transformers
    -- $transformers

    -- * The 'Effect' class
    -- $effect

    -- * The 'MonadEffect' class
    -- $monadeffect

    -- * Runtime implementation
    -- $runtime
    ) where

import Control.Effects
import Control.Effects.State
import Control.Monad.Trans.State as S hiding (State)
import Control.Monad.Trans.Reader as R
import Control.Monad.Trans as T (lift, MonadTrans)
import GHC.Generics
{- $transformers
    Monad transformers are types with the kind @(\* -> \*) -> \* -> \*@ that take a monad as a
    parameter and add some extra functionality on top of it, resulting in a new monad. This isn't a
    tutorial on them so we'll just briefly go through the basics.

    Since a transformer takes a monad and produces a new monad, they can stack on top of each other
    forming \'transformer stacks\'. The functions that utilize a transformer @t@\'s functionality only
    work directly if @t@ is the topmost transformer on the stack. Since there can obviously only be
    one transformer on the top, and we want to use more than one effect, we need to 'T.lift' their
    functions through the stack. For example:
@
'S.get' :: 'Monad' m => 'StateT' s m s
@
    so if we want to use this function in a stack that has a 'ReaderT' transformer on top we need to
    use 'T.lift':

@
'T.lift' 'S.get' :: 'Monad' m => 'ReaderT' r ('StateT' s m) s
@

    If you imagine we had 5 additional layers above the 'ReaderT', we'd need to call 'T.lift' 5 more
    times. Instead we use type classes with polymorphic functions that work over any transformer
    stack. Their instances are arranged in a way that automatically calls 'T.lift' as many times as
    needed. For example, the 'getState' function in @simple-effects@ has the type
    @'MonadEffect' ('State' s) m => m s@. Let's say our stack is
    @'ReaderT' r1 ('ReaderT' r2 ('StateT' s 'IO'))@. The reason why we can use the 'getState'
    function is because there are two instances for @'MonadEffect' ('State' s)@.

@
instance 'MonadEffect' ('State' s) ('StateT' s m)
@

    and

@
instance 'MonadEffect' ('State' s) m => 'MonadEffect' ('State' s) ('ReaderT' r m)
@

    Instance resolution first matches the second instance and sees that it requires a
    @'MonadEffect' ('State' s)@ from the underlying monad @m@. Then it again encounters a
    'ReaderT' and finally it arrives at the 'StateT' layer that has no other requirements.
    The actual implementation for the 'ReaderT' instance does the 'T.lift'ing while the 'StateT'
    implementation actually uses the structure of the 'StateT' transformer to implement the
    required function.

    [Note]
        This is a bit of a simplification. There isn't really a
        @'MonadEffect' ('State' s) ('ReaderT' r m)@ instance, but instead a more general
        @'MonadEffect' e (t m)@ one that works for any transformer and any effect. It's an
        @OVERLAPPABLE@ instance so more specific ones can be chosen if they exist (as is the case
        with, for example, the 'StateT' instance for the 'State' effect)

    Now let's see how a function like 'implementStateViaStateT' works. It's type is
    @'implementStateViaStateT' \@Int :: 'Monad' m => Int -> 'StateT' Int m a -> m a@. Say we also have a
    computation with the type @'MonadEffect' ('State' Int) n => n ()@ that we want to run. If we
    give that computation as the second parameter of the 'implementStateViaStateT' function then @n@
    becomes @'StateT' Int m a@, but since there was also a constraint on the @n@ type, instance
    resolution must check if that type satisfies it. This then finds the
    @instance 'MonadEffect' ('State' s) ('StateT' s m)@ instance and everything works. Then finally
    the actual definition of 'implementStateViaStateT' kicks in and it runs the 'StateT' transformer,
    giving it an initial state value and resulting in a monadic action that's free of the state
    constraint.

    If we had additional effect constraints on our initial computation, then the instance resolution
    would find the matching instances for the 'StateT' transformer that would push the constraint
    onto the underlying monad. This means that our final result wouldn't just have a 'Monad'
    constraint, but also those other ones that remain to be handled.

    What @simple-effects@ does is provide a structured way to define new effects so that they can
    automatically be lifted through monad stacks. Also, for the more complex effects where just
    'lift' isn't enough, it enables the writer of the effect to specify how exactly to lift their
    effect. This is done through the 'Effect' class.
-}
{- $effect
    The core of every effect is it's 'Effect' instance. Here's how the class is defined:

@
class 'Effect' (e :: (* -> *) -> *) where
    type 'CanLift' e (t :: (* -> *) -> * -> *) :: 'Constraint'
    type 'CanLift' e t = 'MonadTrans' t
    'liftThrough' ::
        ('CanLift' e t, 'Monad' m, 'Monad' (t m))
        => e m -> e (t m)
    'mergeContext' :: 'Monad' m => m (e m) -> e m
@

    The type @e@ is the name of your effect and it's where you specify the functionality that your effect
    provides. This will be a record of monadic functions. For example, there's how it's instantiated
    for the state effect:

@
data 'State' s m = 'StateMethods'
    { '_getState' :: m s
    , '_setState' :: s -> m () }
@

    Then there's the 'CanLift' associated type. It specifies the constraint that a transformer needs to satisfy
    to be able to lift the effect through it. Usually, 'MonadTrans' is all you need, so that's the
    default instantiation. Some more complicated effects have tighter demands.

    The 'liftThrough' function gets an implementation of the effect @e@ in the monad @m@ and is
    required to provide an implementation in the monad @t m@. Here's how it might look for the
    'State' effect.

@
'liftThrough' ('StateMethods' g s) = 'StateMethods' ('lift' g) ('lift' . s)
@

    As you can see, 'MonadTrans' is enough in this case since the 'lift' function is all we need.

    Finally, the 'mergeContext' method. Given a record of methods in a monadic context, give me a
    new record of methods that somehow merges that context into it. The way it's implemented is
    simpler than it seems. For example, let's see how we'd implement a function with the following
    signature: @f :: m (a -> m b) -> a -> m b@

    Since our result is monadic we're free to just bind that inner function and give it the extra
    parameter like this:

@
f mamb a = do
    amb <- mamb
    amb a
@

    If we had more than one function inside of the context, we'd just need to select the correct one
    after binding it. Here's how 'mergeContext' is implemented for the 'State' effect.

@
'mergeContext' m = 'StateMethods'
    { '_getState' = do
        sm <- m
        '_getState' sm
    , '_setState' s = do
        sm <- m
        '_setState' sm s }
@

    If these implementations seem pretty mechanical, it's because they are. So mechanical, in fact,
    that in most of the cases you don't even need to write them. Just derive the 'Generic' class
    for your effect and you get those definitions for free. Here's the definition of the @'State' s@
    effect:

@
data 'EffMethods' ('State' s) m = 'StateMethods'
    { '_getState' :: m s
    , '_setState' :: s -> m () }
    deriving ('Generic')
instance 'Effect' ('State' s)
@

    There are a couple of conditions though. The automatic method deriving only works for what
    is called, in the terminology of this library, a simple effect. The methods of a simple effect
    are functions that take arguments that don't depend on the monad @m@ and also have a monadic
    result in that monad. For example, @'setState' :: s -> m ()@ is a method of a simple effect
    while @g :: m a -> m a@ isn't one because the first parameter depends on @m@.

    One extra condition is that there can't be any universally quantified variables in the methods,
    so for example if your effect has a method @h :: forall a. a -> m ()@, you can't derive
    'liftThrough' and 'mergeContext' for it automatically. This isn't because it's impossible
    (or even difficult), but because you can't derive 'Generic' for those types. In those cases
    you need to write the implementations yourself.

    Finally, we have the 'MonadEffect' class.
-}

{- $monadeffect
    It's defined like this

@
class ('Effect' e, 'Monad' m) => 'MonadEffect' e m where
    'effect' :: e m
@

    So it just says that monads that implement the effect @e@ need to provide an implementation of
    the effect's methods that works in that monad.

    As discussed in the transformers section, there's an
    overlappable instance given for the 'MonadEffect' class. Here it is:

@
instance \{\-\# OVERLAPPABLE \#\-\}
    ('MonadEffect' e m, 'Monad' (t m), 'CanLift' e t) => 'MonadEffect' e (t m) where
    'effect' = 'liftThrough' 'effect'
@

    It means that any transformer @t@ can implement the effect @e@ as long as the underlying monad
    can implement it /and/ the transformer satisfies the conditions that the effect imposes on it
    (in most cases, the 'MonadTrans' constraint).

    Transformers that finally implement the effect have specific 'MonadEffect' instances. For
    example:

@
instance 'Monad' m => 'MonadEffect' ('State' s) ('StateT' s m) where
    'effect' = 'StateMethods' 'S.get' 'S.put'
@

    [Note]
        The 'S.get' and 'S.put' functions come from the "Control.Monad.Trans.State" module.
-}

{- $runtime
    Defining effects in a structured way like this has an additional benefit. It lets us use
    implementations that we construct at runtime! This is achieved through the 'RuntimeImplemented'
    type. 'RuntimeImplemented' is a special transformer defined like this:

@
newtype 'RuntimeImplemented' e m a = 'RuntimeImplemented'
    { 'getRuntimeImplemented' :: 'ReaderT' (e m) m a }
@

    It's a wrapper around a 'ReaderT' that carries around an @e m@ implementation.
    @'RuntimeImplemented' e@ has a @'MonadEffect' e@ instance defined like this

@
instance ('Effect' e, 'Monad' m, 'CanLift' e ('RuntimeImplemented' e))
    => 'MonadEffect' e ('RuntimeImplemented' e m) where
    'effect' = 'mergeContext' $ 'RuntimeImplemented' ('liftThrough' <$> 'ask')
@

    Essentially, 'ask' gives us the record, but it's inside of a monadic context so we need
    'mergeContext' to get it out. This lets us implement any effect /at runtime/ using the
    'implement' function:

@
'implement' :: forall e m a. e m -> 'RuntimeImplemented' e m a -> m a
'implement' em ('RuntimeImplemented' r) = 'runReaderT' r em
@

    As with 'implementStateViaStateT', when we give our polymorphic monadic action as a second
    parameter, instance resolution checks if @'RuntimeImplemented' e m a@ has instances for all the
    effects we need. Most of them just propagate through to the inner @m@ monad, but the effect @e@
    that we're implementing finds the instance above and uses that reader's environment to get the
    effect's methods. All that's left is to actually run the 'ReaderT' by giving it the record which
    we're free to construct any way we want.

    [Note]
        One more thing. You might notice that the definition of the state effect doesn't mention the
        'getState' and 'setState' functions. The methods in the record are called '_getState' and
        '_setState', with an underscore. If you think about it you'll see that to actually use one
        of them, we first need to get them out of the record. Since we can access the record of
        methods for the current monad with the 'effect' function, getting the current state would
        look like @'_getState' 'effect'@ so to reduce duplication it's convenient to define helpers
        like @'getState' = '_getState' 'effect'@ and @'setState' = '_setState' effect@. Even simpler,
        though arguably more magical, is the following top level definition

        @
        'StateMethods' 'getState' 'setState' = 'effect'
        @

        When we write that at the top level, we bind the first field in the 'effect' record to the
        'getState' name and the second field to the 'setState' name. It's pretty weird because we're
        not in any specific monad, yet we're still somehow unpacking the record. Still, 'getState',
        for example, happily accepts the @'MonadEffect' ('State' s) m => m s@ type signature and
        it just works.

    That's pretty much it. In the next part we'll take a look at implementing our own effects.
-}
