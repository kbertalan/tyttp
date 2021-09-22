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

