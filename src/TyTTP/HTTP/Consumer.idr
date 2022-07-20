module TyTTP.HTTP.Consumer

import Control.Monad.Trans
import Control.Monad.Either
import Data.Buffer
import Data.Buffer.Ext
import Data.List
import public Data.List.Quantifiers
import Data.Maybe
import Data.SnocList
import Data.IORef
import TyTTP
import TyTTP.HTTP.Protocol

public export
interface Accept t where
  contentType : (ty : Type) -> { auto p : ty = t } -> List String

public export
data IsAccept : (t : Type) -> Type where
  ItIsAccept : Accept t => IsAccept t

public export
ConsumerError : Type
ConsumerError = String

public export
interface Accept t => Consumer a t where
  consumeRaw : (ty : Type) -> { auto p : ty = t } -> (ct : String) -> (raw : Buffer) -> Either ConsumerError a

public export
data IsConsumer : (a : Type) -> (t : Type) -> Type where
  ItIsConsumer : Consumer a t => IsConsumer a t

||| This function consumes the stream from the underlying server, thus the original stream cannot be used twice.
||| If you make sure that the original stream is not used twice, then this function can be used.
export
unsafeConsumeBody : Error e
  => HasIO m
  => MonadPromise e m p
  => (
    Context me u v h1 s h2 Buffer b
    -> (forall p'. MonadPromise e m p' => p' $ Context me' u' v' h1' s' h2' a' b')
  )
  -> Context me u v h1 s h2 (Publisher m e Buffer) b
  -> p $ Context me' u' v' h1' s' h2' a' b'
unsafeConsumeBody handler ctx = promise $ \resolve', reject' => do
  acc <- newIORef Lin
  let subscriber : Subscriber m e Buffer = MkSubscriber
        { onNext = \a => modifyIORef acc (:< a)
        , onSucceded = \_ => do
            all <- concatBuffers =<< asList <$> readIORef acc
            emptyBuffer <- Ext.newBuffer 0
            let result = handler $ { request.body := fromMaybe emptyBuffer all } ctx
            runPromise { m = m } resolve' reject' result
        , onFailed = reject'
        }
  ctx.request.body.subscribe subscriber

consumePayload :
  (t : Type)
  -> (isConsumer : IsConsumer a t)
  -> (ct : String)
  -> (raw : Buffer)
  -> Either ConsumerError a
consumePayload t ItIsConsumer ct raw =
  consumeRaw t ct raw

safeConsume :
  Error e
  => MonadTrans t
  => MonadPromise e IO m
  => Alternative (t m)
  => HasContentType h1
  => (list: List Type)
  -> (areAccepts : All IsAccept list)
  -> (areConsumers : All (IsConsumer a) list)
  -> (ct : String)
  -> (
    Context me u v h1 s h2 (Either ConsumerError a) b
    -> (forall m'. MonadPromise e IO m' => m' $ Context me' u' v' h1' s' h2' a' b')
  )
  -> Context me u v h1 s h2 (Publisher IO e Buffer) b
  -> t m $ Context me' u' v' h1' s' h2' (Publisher IO e Buffer) b'
safeConsume [] _ _ _ _ _ = empty
safeConsume (t::ts) (ItIsAccept::as) (c::cs) ct handler ctx =
  if elem ct (contentType t)
  then lift $ flip unsafeConsumeBody ctx $ \ctx' => promise $ \resolve' ,reject' => do
          let raw = ctx'.request.body
              result = handler $ { request.body := consumePayload t c ct raw } ctx'
              success = \r => resolve' $ { request.body := singleton raw } r
          runPromise { m = IO } success reject' result
  else safeConsume ts as cs ct handler ctx

export
consumes :
  Error e
  => MonadTrans t
  => MonadPromise e IO m
  => Alternative (t m)
  => HasContentType h1
  => (list: List Type)
  -> {auto isNonEmpty : NonEmpty list}
  -> {auto areAccepts : All IsAccept list}
  -> {auto areConsumers : All (IsConsumer a) list}
  -> (
    Context me u v h1 s h2 (Either ConsumerError a) b
    -> (forall m'. MonadPromise e IO m' => m' $ Context me' u' v' h1' s' h2' a' b')
  )
  -> Context me u v h1 s h2 (Publisher IO e Buffer) b
  -> t m $ Context me' u' v' h1' s' h2' (Publisher IO e Buffer) b'
consumes list {isNonEmpty} {areAccepts} {areConsumers} handler ctx = do
  let Just ct = getContentType ctx.request.headers
    | _ => empty

  safeConsume list areAccepts areConsumers ct handler ctx

export
consumes' :
  Error e
  => MonadTrans t
  => MonadPromise e IO m
  => Alternative (t m)
  => HasContentType h1
  => (list: List Type)
  -> {auto isNonEmpty : NonEmpty list}
  -> {auto areAccepts : All IsAccept list}
  -> {auto areConsumers : All (IsConsumer a) list}
  -> (
    Context me u v h1 s h2 ConsumerError b
    -> (forall m'. MonadPromise e IO m' => m' $ Context me' u' v' h1' s' h2' a' b')
  )
  -> (
    Context me u v h1 s h2 a b
    -> (forall m''. MonadPromise e IO m'' => m'' $ Context me' u' v' h1' s' h2' a'' b')
  )
  -> Context me u v h1 s h2 (Publisher IO e Buffer) b
  -> t m $ Context me' u' v' h1' s' h2' (Publisher IO e Buffer) b'
consumes' list {isNonEmpty} {areAccepts} {areConsumers} errHandler handler ctx =
  let handler' : 
        Context me u v h1 s h2 (Either ConsumerError a) b
        -> (forall m'. MonadPromise e IO m' => m' $ Context me' u' v' h1' s' h2' () b')
      handler' s =
        case s.request.body of
          Right r => do
            result <- handler $ { request.body := r } s
            pure $ { request.body := () } result
          Left  l => do
            result <- errHandler $ { request.body := l } s
            pure $ { request.body := () } result
  in consumes list handler' ctx
