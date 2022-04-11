module TyTTP.Core.Request

public export
record Request m u v h a where
  constructor MkRequest
  method : m
  url : u
  version : v
  headers : h
  body : a

export
Functor (Request m u v h) where
  map f req = { body $= f } req

