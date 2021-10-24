module TyTTP.Step

import TyTTP.Request
import TyTTP.Response

public export
record Step me p h1 (fn : me -> Type -> Type) s h2 a b where
  constructor MkStep
  request : Request me p h1 fn a
  response : Response s h2 b

export
Functor (Step me p h1 fn s h2 a) where
  map f step = record { response $= map f } step

export
Bifunctor (Step me p h1 TyTTP.Request.simpleBody s h2) where
  bimap f g step = record { request $= map f, response $= map g } step

