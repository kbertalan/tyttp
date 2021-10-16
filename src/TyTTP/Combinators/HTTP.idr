module TyTTP.Combinators.HTTP

import TyTTP
import TyTTP.HTTP

export
hToPublisher : Applicative m
  => { me : Method }
  -> { a : Type }
  -> { auto methodProof : me = step.request.method }
  -> ( step : Step Method h1 p1 (TyTTP.HTTP.bodyOf {error = e} {monad = m}) s2 h2 a ((TyTTP.HTTP.bodyOf {error = e} {monad = m}) me a) )
  -> m $ Step Method h1 p1 (TyTTP.HTTP.bodyOf {error = e} {monad = m}) s2 h2 a (Publisher m e a)
hToPublisher = \s =>
  let emptyPublisher : Publisher m e a = MkPublisher $ \s => s.onSucceded ()
      originalPublisher : Lazy (Publisher m e a) = believe_me $ TyTTP.Response.Response.body $ s.response
      publisher : Publisher m e a = selectBodyByMethod me emptyPublisher originalPublisher
  in
    pure $ record { response.body = publisher } s
