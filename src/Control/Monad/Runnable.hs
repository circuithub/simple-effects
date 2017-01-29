{-# LANGUAGE TypeFamilies, UndecidableInstances, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
module Control.Monad.Runnable where

import Import
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.Writer.Strict as SW
import qualified Control.Monad.Trans.Writer.Lazy as LW
import qualified Control.Monad.Trans.RWS.Strict as SR
import qualified Control.Monad.Trans.RWS.Lazy as LR
-- import Control.Monad.Trans.Cont -- may be impossible to write

-- | A class of monads that have a run function.
--
--   The 'runMonad' function gives the result inside of IO. The only reason for this is to allow
--   an instance for IO to be written. Other instances do not perform any aditional IO.
--
--   Instances for 'Identity', 'IO' and
--   @('Runnable' m, 'RunnableTrans' t, 'Monad' (t m)) => 'Runnable' (t m)@ are given so users
--   should only provide additional 'RunnableTrans' instances instead of 'Runnable' ones.
class Monad m => Runnable m where
    -- | The type of value that needs to be provided to run this monad.
    type MonadicState m :: *
    -- | The type of the result you get when you run this monad.
    type MonadicResult m a :: *
    -- | Get the current state value.
    currentMonadicState :: m (MonadicState m)
    -- | If given a result, reconstruct a monadic compitation.
    restoreMonadicState :: MonadicResult m a -> m a
    -- | Given the required state value and a computation, run the computation up to the IO effect.
    --   This should effectively run each layer in the transformer stack. The 'MonadicState' should
    --   hold all the needed information to do so.
    --
    --   A more formal description of what it means to run a transformer is given for the
    --   'runTransformer' function.
    runMonad :: MonadicState m -> m a -> IO (MonadicResult m a)

-- | A class of transformers that can run their effects in the underlying monad.
--
--   The following laws need to hold.
--
-- @
--   \t -> do st <- 'currentTransState'
--            res <- 'lift' ('runTransformer' t st)
--            'restoreTransState' res
--   == 'id'
-- @
--
-- @
--   f :: (forall a. m a -> m a)
--   \m s -> runTransformer (lift (f m)) s == \m s -> f (runTransformer (lift m) s)
-- @
class MonadTrans t => RunnableTrans t where
    -- | The type of value that needs to be provided to run this transformer.
    type TransformerState t (m :: * -> *) :: *
    -- | The type of the result you get when you run this transformer.
    type TransformerResult t (m :: * -> *) a :: *
    -- | Get the current state value.
    currentTransState :: Monad m => t m (TransformerState t m)
    -- | If given a result, reconstruct the compitation.
    restoreTransState :: Monad m => TransformerResult t m a -> t m a
    -- | Given the required state value and a computation, run the effects of the transformer
    --   in the underlying monad.
    runTransformer :: Monad m => t m a -> TransformerState t m -> m (TransformerResult t m a)

instance Runnable Identity where
    type MonadicState Identity = ()
    type MonadicResult Identity a = a
    currentMonadicState = return ()
    restoreMonadicState = return
    runMonad _ (Identity a) = return a

instance Runnable IO where
    type MonadicState IO = ()
    type MonadicResult IO a = a
    currentMonadicState = return ()
    restoreMonadicState = return
    runMonad _ m = m

instance (Runnable m, RunnableTrans t, Monad (t m)) => Runnable (t m) where
    type MonadicState (t m) = (TransformerState t m, MonadicState m)
    type MonadicResult (t m) a = MonadicResult m (TransformerResult t m a)
    currentMonadicState = (,) <$> currentTransState <*> lift currentMonadicState
    restoreMonadicState s = lift (restoreMonadicState s) >>= restoreTransState
    runMonad (s, s') t = runMonad s' (runTransformer t s)

instance RunnableTrans (SS.StateT s) where
    type TransformerState (SS.StateT s) m = s
    type TransformerResult (SS.StateT s) m a = (a, s)
    currentTransState = get
    restoreTransState (a, s) = put s >> return a
    runTransformer = SS.runStateT

instance RunnableTrans (LS.StateT s) where
    type TransformerState (LS.StateT s) m = s
    type TransformerResult (LS.StateT s) m a = (a, s)
    currentTransState = get
    restoreTransState (a, s) = put s >> return a
    runTransformer = LS.runStateT

instance Monoid s => RunnableTrans (SW.WriterT s) where
    type TransformerState (SW.WriterT s) m = ()
    type TransformerResult (SW.WriterT s) m a = (a, s)
    currentTransState = return ()
    restoreTransState (a, s) = SW.tell s >> return a
    runTransformer m _ = SW.runWriterT m

instance Monoid s => RunnableTrans (LW.WriterT s) where
    type TransformerState (LW.WriterT s) m = ()
    type TransformerResult (LW.WriterT s) m a = (a, s)
    currentTransState = return ()
    restoreTransState (a, s) = LW.tell s >> return a
    runTransformer m _ = LW.runWriterT m

instance RunnableTrans (ReaderT s) where
    type TransformerState (ReaderT s) m = s
    type TransformerResult (ReaderT s) m a = a
    currentTransState = ask
    restoreTransState = return
    runTransformer = runReaderT

instance Monoid w => RunnableTrans (SR.RWST r w s) where
    type TransformerState (SR.RWST r w s) m = (r, s)
    type TransformerResult (SR.RWST r w s) m a = (a, s, w)
    currentTransState = (,) <$> ask <*> get
    restoreTransState (a, s, w) = SR.tell w >> put s >> return a
    runTransformer m (r, s) = SR.runRWST m r s

instance Monoid w => RunnableTrans (LR.RWST r w s) where
    type TransformerState (LR.RWST r w s) m = (r, s)
    type TransformerResult (LR.RWST r w s) m a = (a, s, w)
    currentTransState = (,) <$> ask <*> get
    restoreTransState (a, s, w) = LR.tell w >> put s >> return a
    runTransformer m (r, s) = LR.runRWST m r s

instance RunnableTrans IdentityT where
    type TransformerState IdentityT m = ()
    type TransformerResult IdentityT m a = a
    currentTransState = return ()
    restoreTransState = return
    runTransformer m () = runIdentityT m

instance Error e => RunnableTrans (ErrorT e) where
    type TransformerState (ErrorT e) m = ()
    type TransformerResult (ErrorT e) m a = Either e a
    currentTransState = return ()
    restoreTransState (Left e) = throwError e
    restoreTransState (Right a) = return a
    runTransformer m () = runErrorT m

instance RunnableTrans (ExceptT e) where
    type TransformerState (ExceptT e) m = ()
    type TransformerResult (ExceptT e) m a = Either e a
    currentTransState = return ()
    restoreTransState (Left e) = throwE e
    restoreTransState (Right a) = return a
    runTransformer m () = runExceptT m

instance RunnableTrans MaybeT where
    type TransformerState MaybeT m = ()
    type TransformerResult MaybeT m a = Maybe a
    currentTransState = return ()
    restoreTransState Nothing = mzero
    restoreTransState (Just a) = return a
    runTransformer m () = runMaybeT m

instance RunnableTrans ListT where
     type TransformerState ListT m = ()
     type TransformerResult ListT m a = [a]
     currentTransState = return ()
     restoreTransState = fromFoldable
     runTransformer m _ = toList m
