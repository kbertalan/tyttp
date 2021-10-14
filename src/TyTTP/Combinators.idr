module TyTTP.Combinators

import TyTTP
import TyTTP.Stream
import Control.Monad.Error.Interface

export
hId : Applicative m => Handler m
  m1 p1 h1 f h2 a b
  m1 p1 h1 f h2 a b
hId = pure . id 

export
hEcho : Applicative m =>
  (step : Step m1 p1 h1 f h2 a b)
  -> m $ Step m1 p1 h1 f h2 a $ f step.request.method a
hEcho step = pure $ map (const step.request.body) step

export
hMapRequest : Applicative m => (i -> o) -> Handler m 
  m1 h1 p1 TyTTP.Request.simpleBody h2 i a
  m1 h1 p1 TyTTP.Request.simpleBody h2 o a
hMapRequest f step = pure $ mapFst f step

export
hMapResponse : Applicative m => (i -> o) -> Handler m
  m1 h1 p1 f h2 a i
  m1 h1 p1 f h2 a o
hMapResponse f step = pure $ map f step

export
hConstRequest : Applicative m => c -> Handler m 
  m1 h1 p1 TyTTP.Request.simpleBody h2 a b
  m1 h1 p1 TyTTP.Request.simpleBody h2 c b
hConstRequest x = hMapRequest $ const x

export
hConstResponse : Applicative m => c -> Handler m
  m1 h1 p1 f h2 a b
  m1 h1 p1 f h2 a c
hConstResponse x = hMapResponse $ const x

export
hParseRequest : MonadError e m => (String -> m a) -> Handler m
  m1 h1 p1 TyTTP.Request.simpleBody h2 String res
  m1 h1 p1 TyTTP.Request.simpleBody h2 a res
hParseRequest parser step = do
  result <- parser step.request.body
  hConstRequest result step

