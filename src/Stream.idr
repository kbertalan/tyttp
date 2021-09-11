module Stream

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

Functor (Publisher m e) where
  map f publisher = MkPublisher $ \s => publisher.subscribe $ contramap f s

mutual

  export
  Monad m => Applicative (Publisher m e) where
    pure a = MkPublisher $ \s => s.onNext a >>= s.onSucceded
    ff <*> a = ff >>= (\f => f <$> a)

  export
  Monad m => Monad (Publisher m e) where
    ma >>= f = MkPublisher $ \s => ma.subscribe $ MkSubscriber
          { onNext = \a =>
              let publisher = f a
                  subscriber = MkSubscriber
                                { onNext = s.onNext }
                                { onSucceded = \_ => pure () }
                                { onFailed = s.onFailed } -- if propagated then multiple error events can be seen in the resulting publisher, ie: no short cirquit behaviour for error cases
              in publisher.subscribe subscriber
          }
          { onSucceded = s.onSucceded }
          { onFailed = s.onFailed }

