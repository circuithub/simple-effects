{-# OPTIONS_GHC -Wno-unused-imports #-}
{-| Let's see how we can implement custom effects. We'll go through a couple of examples of
    increasing complexity. This part does not require going through part 2. That being said,
    understanding the details will help with understanding some of the restrictions and will help
    with compiler errors you might get.
-}
module Tutorial.T3_CustomEffects (
    -- * Files
    -- $files

    -- * Implementations
    -- $implement

    -- * Print
    -- $print

    -- * Fork
    -- $fork
    ) where
import Control.Effects
import Control.Effects.State
import Control.Monad.Trans.State as S hiding (State)
import Control.Monad.Trans.Reader as R
import Control.Monad.Trans as T (lift, MonadTrans)
import GHC.Generics
import Data.Text
import Data.ByteString as B
import Control.Monad.IO.Class
import Control.Concurrent
{- $files
    To start off, we'll define an effect for working with the filesystem. To keep it simple, we'll
    define two functions. One for reading the contents of a file, and one for writing them to a file.

    The first step is to make a name for our effect. Say, @Files@ for example. We declare an empty
    datatype with that name:

@
data Files
@

    Next, we need to provide an instance of the 'Effect' class for our effect.

    The signatures of our functions will be @'FilePath' -> m 'ByteString'@ and
    @'FilePath' -> 'ByteString' -> m ()@ respectively. Both of them are methods of what we call, in the
    jargon of this library, a simple effect. It means that they are functions that return a monadic
    action, and that their arguments don't depend on that monad. As an example, @m Int -> m Int@
    isn't a method of a simple effect because the argument depends on @m@.

    Because of this fact, the instance is super simple:

@
instance 'Effect' Files where
    data 'EffMethods' Files m = FilesMethods
        { _readFile :: 'FilePath' -> m 'ByteString'
        , _writeFile :: 'FilePath' -> 'ByteString' -> m () }
        deriving ('Generic')
@

    The 'Effect' class requires you to provide a record of methods for your effect. This is done
    with an associated data type. You can read about them here
    <https://wiki.haskell.org/GHC/Type_families#Associated_family_declarations>
    The class also has two functions: 'liftThrough' and 'mergeContext'. Luckily, because our effect
    is simple (and mostly they will be), it's enough to just derive 'Generic' for our type. The
    functions are then defined for us.

    It's a convention to name the methods starting with an underscore so we can define the following
    helper functions:

@
readFile :: 'MonadEffect' Files m => 'FilePath' -> m 'ByteString'
writeFile :: 'MonadEffect' Files m => 'FilePath' -> 'ByteString' -> m ()
FilesMethods readFile writeFile = 'effect'
@

    So how does this work, and what does it even do? Well, we defined how a record of methods looks
    for our effect, but how is that record actually constructed? If we wanted to /use/ our methods
    where would we get them from? Enter the 'effect' function. It's type signature is
    @'MonadEffect' e m => 'EffMethods' e m@. It means that for every monad @m@ which supports the
    effect @e@, the 'effect' function gives us a record of the effect's methods. Let's say we want
    to read the contents of a file. First we'd use the 'effect' function to get the methods of our
    @Files@ effect, then we'd get the @_readFile@ function out of it, and finally we'd use that
    function.

@
myFunction = do
    let methods = 'effect'
    let rfFunction = _readFile methods
    rfFunction "somefile.txt"
    -- or equivalently
    _readFile 'effect' "somefile.txt"
@

    Now since writing @_readFile 'effect'@ gets tedious, we can define new top level helper
    functions:

@
readFile = _readFile 'effect'
writeFile = _writeFile 'effect'
@

    The @_readFile@ and @_writeFile@ functions are nothing more than record selectors that get the
    appropriate method from our @FilesMethods@ record. With this in mind we can skip them and just
    directly pattern match on the @FilesMethods@ constructor like

@
FilesMethods readFile writeFile = 'effect'
@

    Check out the previous part if you want to learn more.
-}
{- $implement
    We've only defined the 'syntax' of our effect. How do we actually run those functions? This is
    done through the 'implement' function. It lets us construct the implementations at runtime.

    Suppose we have a function like

@
myFunc :: 'MonadEffect' Files m => m ()
myFunc = do
    file <- readFile "file.dat"
    newFile <- doSomething file
    writeFile "file.dat" newFile
@

    To use it in our program we need to handle the 'MonadEffect' constraint. This is how we might do
    it:

@
main :: IO ()
main = do
    'implement' (FilesMethods 'B.readFile' 'B.writeFile') myFunc
@

    Here we implemented our effect using the 'B.readFile' and 'B.writeFile' functions from the
    "Data.ByteString" module. Of course we're free to implement them however we want. In a testing
    environment we might instead just read/write from a @'Map' 'FilePath' 'ByteString'@ and simulate
    a filesystem.

    To make it easier to use our effect, it's a good idea to provide one or more default handlers.
    For example, we might define two handlers:

@
implementFilesViaIO :: 'MonadIO' m => 'RuntimeImplemented' Files m a -> m a
implementFilesViaMap :: 'Monad' m => 'RuntimeImplemented' Files ('StateT' ('Map' 'FilePath' 'ByteString') m) a -> m a
@

    This way the users of our effect don't have to implement the handlers themselves, but are still free
    to implement more specialized ones.
-}
{- $print
    For our next effect we'll do logging. Just a simple printing function that takes anything
    with a 'Show' instance and logs it somewhere.

    The signature we want is @print :: ('MonadEffect' Print m, 'Show' a) => a -> m ()@. Now here's
    the main issue. The @a@ variable isn't mentioned anywhere in the effect. After all, we don't
    want a separate effect for each possible type. We want the @Print@ effect to provide printing
    /for all/ types with a 'Show' instance. To this end we'll use the @RankNTypes@ extension and define
    our 'Effect' instance like this:

@
data Print

instance 'Effect' Print where
    data 'EffMethods' Print m = PrintMethods
        { _print :: forall a. Show a => a -> m () }
@

    Notice we didn't derive 'Generic'. This is because we can't. Despite our effect being simple
    (the function's parameter doesn't depend on @m@ and it returns a monadic action), the @forall@
    in there makes it impossible to derive a 'Generic' instance. Unfortunately, this means that we
    have to implement the two functions of the 'Effect' class ourselves. Fortunately, it's a very
    mechanical procedure (that's why they can usually be automatically derived!).

    The two functions are

@
'liftThrough' :: ('MonadTrans' t, 'Monad' m, 'Monad' (t m)) => 'EffMethods' e m -> 'EffMethods' e (t m)
@

    and

@
'mergeContext' :: 'Monad' m => m ('EffMethods' e m) -> 'EffMethods' e m
@

    [Note]
        The 'MonadTrans' part is a slight simplification, but it's an honest one for our current
        example. You can read more about the actual definitions and the purpose of these two
        functions in the previous part of the tutorial.

    The first function, 'liftThrough', takes a record of methods of the effect @e@ for the monad
    @m@, and is expected to return a new record, but this time for the monad @t m@. Two puzzle
    pieces make this very easy to do. The first is the fact that the only place where @m@ is mentioned
    in our effect is in the result of the @_print@ function. The second piece of the puzzle is the
    @'lift' :: ('MonadTrans' t, 'Monad' m) => m a -> (t m) a@ function of the 'MonadTrans' class. So to
    construct the new @_print@ method, we just call the old one and 'lift' the result:

@
'liftThrough' (PrintMethods pr) = PrintMethods (\\a -> 'lift' (pr a))
@

    This will work for as many methods with as many parameters as you want. The implementation will
    always look something like

@
'liftThough' (MyMethods m1 m2 m3 m4) = MyMethods
    (\\a -> 'lift' (m1 a))
    (\\a b c -> 'lift' (m2 a b c))
    (\\a b -> 'lift' (m3 a b))
    ('lift' m4)
@

    Up next: 'mergeContext'. It says that given the record of methods /inside a monadic context/,
    give me just the record, somehow pushing that context inside of it. The implementation is again
    very mechanical.

@
'mergeContext' pm = PrintMethods
    (\\a -> do
        PrintMethods p <- pm
        p a)
@

    Essentially, we just pass the parameters through to the old record, but first we must actually
    get the old record out of the monadic context. We can do that because each method's result is
    a monadic action. Here's how it would look like for a bigger effect:

@
'mergeContext' mm = MyMethods
    (\\a -> do
        MyMethods m _ _ _ <- mm
        m a)
    (\\a b c -> do
        MyMethods _ m _ _ <- mm
        m a b c)
    (\\a b -> do
        MyMethods _ _ m _ <- mm
        m a b)
    (do
        MyMethods _ _ _ m <- mm
        m)
@

    [Note]
        Instead of pattern matching, we can use the name of our method as a field selector:

        @
        'mergeContext' pm = PrintMethods
            (\\a -> do
                m <- pm
                _print m a)
        @

    As you can see, there isn't much to these implementations. Just boilerplate. Also note that
    while our @Print@ effect may seem simple, it's actually more complicated than it needs to be.
    Instead we could have defined the whole thing like this:

@
data Print
instance 'Effect' Print where
    data 'EffMethods' Print m = PrintMethods
        { _printString :: 'String' -> m () }
        deriving ('Generic')

print :: ('MonadEffect' Print m, 'Show' a) => a -> m ()
print = _printString 'effect' . 'show'
@

    That way we still get a nice polymorhpic function, but the effect itself is monomorphic and
    lets us get away with just deriving 'Generic'.

    Next, we'll look at a non-simple effect. One for which the 'liftThrough' method can't be
    derived because there isn't just a single valid implementation.
-}
{- $fork
    Here's the challenge. There's a 'forkIO' function in base with the following signature

@
'forkIO' :: IO () -> IO 'ThreadId'
@

    We want to generalize this function to work with monads other than 'IO'. Essentially, we want

@
fork :: 'MonadEffect' Fork m => m () -> m ('Maybe' 'ThreadId')
@

    [Note]
        The 'Maybe' part is so we don't get tied down to 'IO'. Don't worry about it for now.

    Notice that this isn't a simple effect as the parameter is a monadic action. Anyways, let's try
    defining our effect and see where we get stuck:

@
data Fork
instance 'Effect' Fork where
    data 'EffMethods' Fork m = ForkMethods
        { _fork :: m () -> m ('Maybe' 'ThreadId') }
    'mergeContext' mm = ForkMethods
        (\\a -> do
            ForkMethods m <- mm
            m a)
    'liftThrough' (ForkMethods f) = ForkMethods
        (\\a -> 'lift' (f a))
@

    Simple right? Well, unfortunately it doesn't typecheck. The problem is in the 'liftThrough'
    function. Here are the relevant types:

@
f :: m () -> m 'ThreadId'
a :: t m ()
@

    The result we need is of type @t m 'ThreadId'@. If we could somehow get a @m 'ThreadId'@ we'd
    be fine since just 'lift'ing that does the trick. The problem is, the only way to get a
    @m 'ThreadId'@ is by calling @f@ with a @m ()@, and we don't have that. What we do have is
    @a :: t m ()@ so it seems that we need a function that's opposite of 'lift'. Something like
    @unlift :: t m a -> m a@.


    Turns out, that's not so simple to do. Imagine you have a function like with a type @a -> m b@.
    In this case the @a ->@ part is @t@. If we specialize @unlift@ to that we get
    @unlift :: (a -> m b) -> m b@. There's no way to implement that function. To get @m b@ we need
    to have an @a@, but none are given to us.

    In any case, even if @unlift@ was possible to implement, it's not like we could use it. The only
    thing we know about @t@ is that it has a 'MonadTrans' instance (that's where we get the 'lift'
    function from)... Well, not exactly. Remember that note about 'MonadTrans' being a
    simplification? The 'Effect' class actually has an additional associated type. It lets us
    require a custom constraint for our transformer so we can actually require @t@ to be an instance
    of anything we like.

    [Note]
        This extra power doesn't come for free, though. Stricter requirements mean that your effect can't
        be used in certain situations. What this means exactly is a bit out of the scope of this tutorial,
        but here's a quick rundown.

        Handling effects requires monad transformers. Each effect handled will result in at least one
        extra transformer on your transformer stack. Those transformers are the types that need to
        satisfy the requirements of your effect. Having 'MonadTrans' as a requirement is basically
        free since each transformer has a 'MonadTrans' instance, kind of by definition. Anything
        extra and things get a bit more complicated. To \"lift\" functions like 'forkIO' into other
        transformers, people usually use the 'MonadTransControl' class from the "monad-control"
        package. Pretty much all the standard transformers are instances of that class, with one
        exception: 'ContT' isn't an instance of 'MonadTransControl'. 'ContT' is a pretty exotic
        transformer though.

    What we're going to use here is the 'RunnableTrans' class. This is an alternative to the
    'MonadTransControl' class that's hopefully a bit easier to use. It lets us \"run\" a transformer
    if we give it the right state value. It also lets us get the current state value from the context
    and it can restore the context from the result of running the transformer. To cut through the
    confusion (or perhaps to introduce more of it) here's the code:

@
data Fork
instance 'Effect' Fork where
    data 'EffMethods' Fork m = ForkMethods
        { _fork :: m () -> m ('Maybe' 'ThreadId') }
    type 'CanLift' Fork t = 'RunnableTrans'  t
    'mergeContext' mm = ForkMethods
        (\\a -> do
            ForkMethods m <- mm
            m a)
    'liftThrough' (ForkMethods f) = ForkMethods
        (\\a -> do
            st <- 'currentTransState'
            'lift' (f ('void' ('runTransformer' a st)))
            )
@

    Essentially what we do here is get the current state from the main computation (the one doing
    the forking), using that state to run the forked computation, discard both its result /and/
    it's state using the 'void' function, then we finally call the original function and 'lift' the
    whole thing.

    [Note]
        Since we're discarding the result and the state of the forked computation, and this will
        happen for each transformer/effect in the stack, it means that only effects that \"survive\"
        are the final 'IO' ones. The state of the original computation does get shared with the
        forked one, so that's pretty useful, but if we care about what the forked computation /did/
        with that state, we need to communicate with the original thread manually through some 'IO'
        mechanism like 'MVar's.

    What about the effect handlers? How do we write one for our @Fork@ effect? Here's one that
    ignores completely what the intended semantics were and just runs the thing sequentially:

@
implementForkSequentially :: Monad m => RuntimeImplemented Fork m a -> m a
implementForkSequentially = 'implement' (ForkMethods (\c -> c >> 'return' 'Nothing'))
@

    But to /really/ fork the computation we have to use the original 'forkIO' function like this:

@
implementForkIO :: RuntimeImplemented Fork IO a -> IO a
implementForkIO = 'implement' (ForkMethods ('fmap' 'Just' . 'forkIO'))
@

    Notice that this forces our monad to IO. This means no other effects can be handled after it.
    This is manageable if @Fork@ is the only effect with that condition, but what if there are more
    'IO' based ones? A solution is to provide a @'MonadEffect' Fork IO@ instance directly

@
instance 'MonadEffect' Fork IO where
    effect = ForkMethods ('fmap' 'Just' . 'liftIO')
@

    Now we can write @implementForkIO@ like this

@
implementForkIO :: IO a -> IO a
implementForkIO = 'id'
@

    As you can see, the function doesn't do anything, but it does force whatever we give it to be
    in the 'IO' monad. This again means that we must handle this effect after all others, but if
    there are other effects with the same requirement, they can all be handled at the end.
-}