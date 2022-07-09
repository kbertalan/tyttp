module TyTTP.Core.Promise

import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

public export
record Callbacks e (m : Type -> Type) a where
  constructor MkCallbacks
  onSucceded : a -> m ()
  onFailed : e -> m ()

public export
record Promise e (m : Type -> Type) a where
  constructor MkPromise
  continuation : Callbacks e m a -> m ()

export %inline
runPromise : (a -> m ()) -> (e -> m ()) -> Promise e m a -> m ()
runPromise onSucceded onFailed promise = continuation promise $ MkCallbacks onSucceded onFailed

export
succeed : a -> Promise e m a
succeed a = MkPromise $ \cb => cb.onSucceded a

export
fail : e -> Promise e m a
fail e = MkPromise $ \cb => cb.onFailed e

export
mapFailure : (e -> e') -> Promise e m a -> Promise e' m a
mapFailure fn (MkPromise conta) = MkPromise $ \cb => conta $ MkCallbacks
  { onSucceded = cb.onSucceded
  , onFailed = cb.onFailed . fn
  }

export
Functor (Promise e m) where
  map f (MkPromise ca) = MkPromise $ \cb => ca $ MkCallbacks
    { onSucceded = cb.onSucceded . f
    , onFailed = cb.onFailed
    }

mutual

  export
  Applicative (Promise e m) where
    pure = succeed
    fn <*> pa = fn >>= \f => map f pa

  export
  Monad (Promise e m) where
    (>>=) (MkPromise conta) f = MkPromise $ \cb => conta $ MkCallbacks
        { onSucceded = \a =>
            let MkPromise contb = f a
            in contb cb
        , onFailed = cb.onFailed
        }

export
MonadTrans (Promise e) where
  lift ma = MkPromise $ \cb => ma >>= cb.onSucceded

export
HasIO m => HasIO (Promise e m) where
  liftIO = lift . liftIO

export
MonadError e (Promise e m) where
  throwError = fail
  catchError (MkPromise conta) fn = MkPromise $ \cb => conta $ MkCallbacks
    { onSucceded = cb.onSucceded
    , onFailed = \e =>
        let (MkPromise contb) = fn e
        in contb $ MkCallbacks
          { onSucceded = cb.onSucceded
          , onFailed = cb.onFailed
          }
    }

public export
interface Monad m => Monad n => MonadPromise e n m | m where
  promise : ((resolve: a -> n ()) -> (reject: e -> n ()) -> n ()) -> m a

export
resolve : MonadPromise e n m => a -> m a
resolve = pure

export
reject : MonadPromise e n m => e -> m a
reject e = promise $ \_, reject' => reject' e

public export
Monad n => MonadPromise e n (Promise e n) where
  promise fn = MkPromise $ \cb => fn cb.onSucceded cb.onFailed

public export
MonadPromise e n m => MonadPromise e n (EitherT e' m) where
  promise fn = MkEitherT $ promise $ \resolve, reject => fn (resolve . Right) reject

public export
MonadPromise e n m => MonadPromise e n (MaybeT m) where
  promise fn = MkMaybeT $ promise $ \resolve, reject => fn (resolve . Just) reject

public export
MonadPromise e n m => MonadPromise e n (RWST r w s m) where
  promise fn = MkRWST $ \r,s,w => promise $ \resolve, reject => fn (\a => resolve (a, (s, w))) reject

public export
MonadPromise e n m => MonadPromise e n (ReaderT r m) where
  promise fn = MkReaderT $ \r => promise fn

public export
MonadPromise e n m => MonadPromise e n (StateT s m) where
  promise fn = ST $ \s => promise $ \resolve, reject => fn (\a => resolve (s,a)) reject

public export
MonadPromise e n m => MonadPromise e n (WriterT w m) where
  promise fn = MkWriterT $ \w => promise $ \resolve, reject => fn (\a => resolve (a,w)) reject
