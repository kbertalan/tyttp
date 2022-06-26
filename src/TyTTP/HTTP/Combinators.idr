module TyTTP.HTTP.Combinators

import Control.Monad.Trans
import Data.Buffer
import Data.Buffer.Ext
import Data.Maybe
import Data.SnocList
import Data.IORef
import TyTTP
import TyTTP.HTTP

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

