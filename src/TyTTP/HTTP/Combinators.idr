module TyTTP.HTTP.Combinators

import Control.Monad.Trans
import Data.Buffer
import Data.Buffer.Ext
import Data.Maybe
import Data.SnocList
import Data.IORef
import TyTTP
import TyTTP.HTTP

export
hToPublisher : Applicative m
  => { me : Method }
  -> { a : Type }
  -> { auto methodProof : me = step.request.method }
  -> ( step : Step Method h1 u (TyTTP.HTTP.bodyOf {error = e, monad = m}) s h2 a ((TyTTP.HTTP.bodyOf {error = e, monad = m}) me a) )
  -> m $ Step Method h1 u (TyTTP.HTTP.bodyOf {error = e} {monad = m}) s h2 a (Publisher m e a)
hToPublisher step =
  let originalPublisher : Lazy (Publisher m e a) = believe_me $ Response.body $ step.response
      publisher : Publisher m e a = selectBodyByMethod me empty originalPublisher
  in
    pure $ record { response.body = publisher } step

||| This function consumes the stream from the underlying server, thus the original stream cannot be used twice.
||| If you make sure that the original stream is not used twice, then this function can be used.
export
unsafeConsumeBody : Error e
  => HasIO m
  => (
    Step Method u h1 Request.simpleBody s h2 Buffer b
    -> Promise e m $ Step Method u' h1' Request.simpleBody s' h2' a' b'
  )
  -> Step Method u h1 (HTTP.bodyOf {monad = IO, error = e}) s h2 Buffer b
  -> Promise e m $ Step Method u' h1' (HTTP.bodyOf {monad = IO, error = e}) s' h2' a' b'
unsafeConsumeBody handler step = MkPromise $ \cb => do
  acc <- newIORef Lin
  let handlerCallbacks = MkCallbacks
        { onSucceded = \r => cb.onSucceded $ { request.body := mkRequestBody r.request.method $ singleton r.request.body } r
        , onFailed = cb.onFailed
        }
      subscriber : Subscriber m e Buffer = MkSubscriber
        { onNext = \a => modifyIORef acc (:< a)
        , onSucceded = \_ => do
            all <- concatBuffers =<< asList <$> readIORef acc
            emptyBuffer <- Ext.newBuffer 0
            let result = handler $ { request.body := fromMaybe emptyBuffer all } step
            result.continuation handlerCallbacks
        , onFailed = cb.onFailed
        }
      withBody : Lazy (m ()) = (believe_me step.request.body).subscribe subscriber
      withoutBody : Lazy (m ()) = empty.subscribe subscriber
  selectBodyByMethod step.request.method withoutBody withBody

