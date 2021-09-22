module TyTTP.Response

public export
data Status = OK | Moved | BadRequest | InternalError

public export
record Response h a where
  constructor MkResponse
  status : Status 
  headers : h
  body : a

export
Functor (Response h) where
  map f res = record { body $= f } res

