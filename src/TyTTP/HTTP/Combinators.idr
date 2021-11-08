module TyTTP.HTTP.Combinators

import Data.Buffer
import Data.IORef
import Node.Buffer
import Node.Error
import TyTTP
import TyTTP.HTTP
import TyTTP.Support.Promise

export
hToPublisher : Applicative m
  => { me : Method }
  -> { a : Type }
  -> { auto methodProof : me = step.request.method }
  -> ( step : Step Method h1 u (TyTTP.HTTP.bodyOf {error = e} {monad = m}) s h2 a ((TyTTP.HTTP.bodyOf {error = e} {monad = m}) me a) )
  -> m $ Step Method h1 u (TyTTP.HTTP.bodyOf {error = e} {monad = m}) s h2 a (Publisher m e a)
hToPublisher = \s =>
  let originalPublisher : Lazy (Publisher m e a) = believe_me $ Response.body $ s.response
      publisher : Publisher m e a = selectBodyByMethod me empty originalPublisher
  in
    pure $ record { response.body = publisher } s

export
consumeBody : HasIO m
  => (
    Step Method u h1 Request.simpleBody s h2 String b
    -> m $ Step Method u' h1' Request.simpleBody s' h2' String b'
  )
  -> Step Method u h1 (HTTP.bodyOf {monad = IO, error = NodeError}) s h2 Buffer b
  -> Promise NodeError m $ Step Method u' h1' (HTTP.bodyOf {monad = IO, error = NodeError}) s' h2' Buffer b'
consumeBody handler step = MkPromise $ \cont => do
  acc <- newIORef ""
  let subscriber = MkSubscriber
        { onNext = \a => do
            modifyIORef acc (<+> (Buffer.toStringUTF8 a)) }
        { onSucceded = \_ => do
            all <- readIORef acc
            result <- handler $ { request.body := all } step
            cont.onSucceded $ { request.body := mkRequestBody result.request.method $ singleton $ fromString result.request.body } result
        }
        { onFailed = cont.onFailed }
      withBody : Lazy (m ()) = (believe_me step.request.body).subscribe subscriber
      withoutBody : Lazy (m ()) = (singleton "").subscribe subscriber
  selectBodyByMethod step.request.method withoutBody withBody

