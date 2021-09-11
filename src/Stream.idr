module Stream

public export
record Subscriber (m : Type -> Type) (e : Type) (a : Type) where
  constructor MkSubscriber
  onNext: a -> m ()
  onSucceded: () -> m ()
  onFailed: e -> m ()

public export
record Publisher (m : Type -> Type) (e : Type) (a : Type) where
  constructor MkPublisher
  subscribe : Subscriber m e a -> m ()

