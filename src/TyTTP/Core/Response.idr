module TyTTP.Core.Response

public export
record Response s h a where
  constructor MkResponse
  status : s 
  headers : h
  body : a

export
Functor (Response s h) where
  map f res = { body $= f } res

