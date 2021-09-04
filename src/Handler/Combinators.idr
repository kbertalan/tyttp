module Handler.Combinators

import Handler
import Control.Monad.Error.Interface

export
hId : Applicative m => Handler m a b a b
hId = pure . id

export
hEcho : Applicative m => Handler m a b a a
hEcho step = pure $ mapSnd (const step.request.body) step

export
hMapRequest : Applicative m => (i -> o) -> Handler m i a o a
hMapRequest f step = pure $ mapFst f step

export
hMapResponse : Applicative m => (i -> o) -> Handler m a i a o
hMapResponse f step = pure $ mapSnd f step

export
hConstRequest : Applicative m => c-> Handler m a b c b
hConstRequest x = hMapRequest $ const x

export
hConstResponse : Applicative m => c -> Handler m a b a c
hConstResponse x = hMapResponse $ const x

export
hParseRequest : MonadError e m => (String -> m a) -> Handler m String res a res
hParseRequest parser step = do
  result <- parser step.request.body
  hConstRequest result step

