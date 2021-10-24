module TyTTP.Request

public export
simpleBody : { 0 m : Type } -> m -> Type -> Type
simpleBody m a = a

public export
record Request m u h (f : m -> Type -> Type) a where
  constructor MkRequest
  method : m
  url : u
  headers : h
  body : f method a

export
Functor (Request m u h TyTTP.Request.simpleBody) where
  map f req = record { body $= f } req

