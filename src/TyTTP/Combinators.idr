module TyTTP.Combinators

import TyTTP.Handler
import TyTTP.Request
import TyTTP.Response
import TyTTP.Stream
import Control.Monad.Error.Interface

export
hId : Applicative m => Handler m
  m1 h1 f h2 a b
  m1 h1 f h2 a b
hId = pure . id 

export
hEcho : Applicative m =>
  (step : Step m1 h1 f h2 a b)
  -> m $ Step m1 h1 f h2 a $ f step.request.method a
hEcho step = pure $ map (const step.request.body) step

export
hMapRequest : Applicative m => (i -> o) -> Handler m 
  m1 h1 TyTTP.Request.simpleBody h2 i a
  m1 h1 TyTTP.Request.simpleBody h2 o a
hMapRequest f step = pure $ mapFst f step

export
hMapResponse : Applicative m => (i -> o) -> Handler m
  m1 h1 f h2 a i
  m1 h1 f h2 a o
hMapResponse f step = pure $ map f step

export
hConstRequest : Applicative m => c -> Handler m 
  m1 h1 TyTTP.Request.simpleBody h2 a b
  m1 h1 TyTTP.Request.simpleBody h2 c b
hConstRequest x = hMapRequest $ const x

export
hConstResponse : Applicative m => c -> Handler m
  m1 h1 f h2 a b
  m1 h1 f h2 a c
hConstResponse x = hMapResponse $ const x

export
hParseRequest : MonadError e m => (String -> m a) -> Handler m
  m1 h1 TyTTP.Request.simpleBody h2 String res
  m1 h1 TyTTP.Request.simpleBody h2 a res
hParseRequest parser step = do
  result <- parser step.request.body
  hConstRequest result step

namespace HTTP

  export
  hToPublisher : Applicative m
    => { me : Method }
    -> { a : Type }
    -> { auto methodProof : me = step.request.method }
    -> ( step : Step Method h1 (TyTTP.Request.httpBodyOf {error = e} {monad = m}) h2 a ((TyTTP.Request.httpBodyOf {error = e} {monad = m}) me a) )
    -> m $ Step Method h1 (TyTTP.Request.httpBodyOf {error = e} {monad = m}) h2 a (Publisher m e a)
  hToPublisher = \s =>
    let emptyPublisher : Publisher m e a = MkPublisher $ \s => s.onSucceded ()
        originalPublisher : Lazy (Publisher m e a) = believe_me $ TyTTP.Response.Response.body $ s.response
        publisher =
          case me of
            OPTIONS => emptyPublisher
            GET => emptyPublisher
            HEAD => emptyPublisher
            POST => originalPublisher
            PUT => originalPublisher
            DELETE => emptyPublisher
            TRACE => emptyPublisher
            CONNECT => emptyPublisher
            OtherMethod _ => originalPublisher
    in
      hConstResponse publisher s
