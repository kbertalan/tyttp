module TyTTP.Core.Step

import Control.Monad.Trans
import TyTTP.Core.Request
import TyTTP.Core.Response

public export
record Step me u h1 (fn : me -> Type -> Type) s h2 a b where
  constructor MkStep
  request : Request me u h1 fn a
  response : Response s h2 b

export
Functor (Step me u h1 fn s h2 a) where
  map f step = record { response $= map f } step

export
Bifunctor (Step me u h1 Request.simpleBody s h2) where
  bimap f g step = record { request $= map f, response $= map g } step

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
