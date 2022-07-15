module TyTTP.HTTP.Consumer

import Control.Monad.Trans
import Control.Monad.Either
import Data.Buffer
import Data.List
import Data.List.Quantifiers
import TyTTP
import TyTTP.HTTP
import TyTTP.HTTP.Combinators

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
