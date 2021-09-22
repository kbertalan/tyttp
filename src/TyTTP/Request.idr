module TyTTP.Request

public export
record Request h a where
  constructor MkRequest
  headers : h
  body : a

export
Functor (Request h) where
  map f req = record { body $= f } req

