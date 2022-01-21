module TyTTP.Core.Request

public export
record Request m u h a where
  constructor MkRequest
  method : m
  url : u
  headers : h
  body : a

export
Functor (Request m u h) where
  map f req = { body $= f } req

