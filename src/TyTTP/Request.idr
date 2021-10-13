module TyTTP.Request

public export
simpleBody : { 0 m : Type } -> m -> Type -> Type
simpleBody m a = a

public export
record Request m h (f : m -> Type -> Type) a where
  constructor MkRequest
  method : m
  headers : h
  body : f method a

export
Functor (Request m h TyTTP.Request.simpleBody) where
  map f req = record { body $= f } req

