module TyTTP.Core.Step

import Control.Monad.Trans
import TyTTP.Core.Request
import TyTTP.Core.Response

public export
record Step me u h1 s h2 a b where
  constructor MkStep
  request : Request me u h1 a
  response : Response s h2 b

export
Functor (Step me u h1 s h2 a) where
  map f step = { response $= map f } step

export
Bifunctor (Step me u h1 s h2) where
  bimap f g step = { request $= map f, response $= map g } step

infixr 0 :>, :>>

export
(:>) : MonadTrans t
  => Monad m
  => (f : (a -> (t m) b) -> c)
  -> (handler : a -> m b)
  -> c
(:>) f handler = f $ \a => lift $ handler a

export
(:>>) : MonadTrans t1
  => MonadTrans t2
  => Monad m
  => Monad (t1 m)
  => (f : (a -> (t2 (t1 m)) b) -> c)
  -> (handler : a -> m b)
  -> c
(:>>) f handler = f $ \a => lift $ lift $ handler a
