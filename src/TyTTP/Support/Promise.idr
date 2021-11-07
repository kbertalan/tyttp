module TyTTP.Support.Promise

import Control.Monad.Trans

public export
record Callbacks e (m : Type -> Type) a where
  constructor MkCallbacks
  onSucceded : a -> m ()
  onFailed : e -> m ()

public export
record Promise e (m : Type -> Type) a where
  constructor MkPromise
  continuation : Callbacks e m a -> m ()

export
Functor (Promise e m) where
  map f (MkPromise ca) = MkPromise $ \cb => ca $ MkCallbacks (cb.onSucceded . f) cb.onFailed

mutual

  export
  Applicative (Promise e m) where
    pure a = MkPromise $ \cont => cont.onSucceded a
    fn <*> pa = fn >>= \f => map f pa

  export
  Monad (Promise e m) where
    (>>=) (MkPromise conta) f = MkPromise $ \cbb => conta $ MkCallbacks
        { onSucceded = \a =>
            let MkPromise contb = f a
            in contb cbb
        }
        { onFailed = cbb.onFailed }

export
MonadTrans (Promise e) where
  lift ma = MkPromise $ \cont => ma >>= cont.onSucceded
