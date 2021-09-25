module TyTTP.Request

public export
data Method
  = OPTIONS
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | TRACE
  | CONNECT
  | OtherMethod String

export
parse : String -> Method
parse str = case str of
  "OPTIONS" => OPTIONS
  "GET" => GET
  "HEAD" => HEAD
  "POST" => POST
  "PUT" => PUT
  "DELETE" => DELETE
  "TRACE" => TRACE
  "CONNECT" => CONNECT
  s => OtherMethod s

public export
record Request h a where
  constructor MkRequest
  method : Method
  headers : h
  body : a

export
Functor (Request h) where
  map f req = record { body $= f } req

