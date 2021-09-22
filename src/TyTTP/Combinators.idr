module TyTTP.Combinators

import TyTTP.Handler
import TyTTP.Request
import Control.Monad.Error.Interface

export
hId : Applicative m => Handler m h1 h2 a b h1 h2 a b
hId = pure . id

export
hEcho : Applicative m => Handler m h1 h2 a b h1 h2 a a
hEcho step = pure $ mapSnd (const step.request.body) step

export
hMapRequest : Applicative m => (i -> o) -> Handler m h1 h2 i a h1 h2 o a
hMapRequest f step = pure $ mapFst f step

export
hMapResponse : Applicative m => (i -> o) -> Handler m h1 h2 a i h1 h2 a o
hMapResponse f step = pure $ mapSnd f step

export
hConstRequest : Applicative m => c-> Handler m h1 h2 a b h1 h2 c b
hConstRequest x = hMapRequest $ const x

export
hConstResponse : Applicative m => c -> Handler m h1 h2 a b h1 h2 a c
hConstResponse x = hMapResponse $ const x

export
hParseRequest : MonadError e m => (String -> m a) -> Handler m h1 h2 String res h1 h2 a res
hParseRequest parser step = do
  result <- parser step.request.body
  hConstRequest result step

