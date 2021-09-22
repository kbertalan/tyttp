module TyTTP.Handler

import TyTTP.Request
import TyTTP.Response

public export
record Step ha hb (a : Type) (b : Type) where
  constructor MkStep
  request : Request ha a
  response : Response hb b

export
Bifunctor (Step ha hb) where
  bimap f g step = record { request $= map f, response $= map g } step

public export
Handler : (Type -> Type) -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
Handler m ha hb a b hc hd c d = Step ha hb a b -> m (Step hc hd c d)

