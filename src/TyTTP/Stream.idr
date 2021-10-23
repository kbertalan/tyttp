module TyTTP.Stream

import Data.Contravariant

public export
record Subscriber (m : Type -> Type) (e : Type) (a : Type) where
  constructor MkSubscriber
  onNext: a -> m ()
  onSucceded: () -> m ()
  onFailed: e -> m ()

export
Contravariant (Subscriber m e) where
  contramap f subscriber = { onNext := subscriber.onNext . f } subscriber

public export
record Publisher (m : Type -> Type) (e : Type) (a : Type) where
  constructor MkPublisher
  subscribe : Subscriber m e a -> m ()

export
Functor (Publisher m e) where
  map f publisher = MkPublisher $ \s => publisher.subscribe $ contramap f s

export
empty : Publisher m e a
empty = MkPublisher $ \s => s.onSucceded ()

export
fail : e -> Publisher m e a
fail e = MkPublisher $ \s => s.onFailed e

export
singleton : Monad m => a -> Publisher m e a
singleton a = MkPublisher $ \s => s.onNext a >>= s.onSucceded

export
fromList : Monad m => List a -> Publisher m e a
fromList list = MkPublisher $ \s => do
  traverse_ s.onNext list
  s.onSucceded ()

