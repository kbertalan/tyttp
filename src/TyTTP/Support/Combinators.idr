module TyTTP.Support.Combinators

import TyTTP
import Control.Monad.Error.Interface

export
hId : Applicative m
  => Step me u h1 fn s h2 a b
  -> m $ Step me u h1 fn s h2 a b
hId = pure . id 

export
hEcho : Applicative m
  => (step : Step me u h1 fn s h2 a b)
  -> m $ Step me u h1 fn s h2 a $ fn step.request.method a
hEcho step = pure $ map (const step.request.body) step

export
hMapRequest : Applicative m
  => (a -> a')
  -> Step me h1 u Request.simpleBody s h2 a b
  -> m $ Step me h1 u Request.simpleBody s h2 a' b
hMapRequest f step = pure $ mapFst f step

export
hMapResponse : Applicative m
  => (b -> b')
  -> Step me h1 u fn s h2 a b
  -> m $ Step me h1 u fn s h2 a b'
hMapResponse f step = pure $ map f step

export
hConstRequest : Applicative m
  => a'
  -> Step me h1 u Request.simpleBody s h2 a b
  -> m $ Step me h1 u Request.simpleBody s h2 a' b
hConstRequest x = hMapRequest $ const x

export
hConstResponse : Applicative m
  => b'
  -> Step me h1 u fn s h2 a b
  -> m $ Step me h1 u fn s h2 a b'
hConstResponse x = hMapResponse $ const x

export
hParseRequest : MonadError e m
  => (String -> m a')
  -> Step me h1 u Request.simpleBody s h2 String b
  -> m $ Step me h1 u Request.simpleBody s h2 a' b
hParseRequest parser step = do
  result <- parser step.request.body
  hConstRequest result step

