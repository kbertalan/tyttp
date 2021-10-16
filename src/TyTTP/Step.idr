module TyTTP.Step

import TyTTP.Request
import TyTTP.Response

public export
record Step ma pa ha (f : ma -> Type -> Type) sb hb a b where
  constructor MkStep
  request : Request ma pa ha f a
  response : Response sb hb b

export
Functor (Step ma pa ha f sb hb a) where
  map f step = record { response $= map f } step

export
Bifunctor (Step ma pa ha TyTTP.Request.simpleBody sb hb) where
  bimap f g step = record { request $= map f, response $= map g } step

