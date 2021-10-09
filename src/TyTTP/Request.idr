module TyTTP.Request

import TyTTP.Stream

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
Eq Method where
  (==) OPTIONS OPTIONS = True
  (==) GET     GET = True
  (==) HEAD    HEAD = True
  (==) POST    POST = True
  (==) PUT     PUT = True
  (==) DELETE  DELETE = True
  (==) TRACE   TRACE = True
  (==) CONNECT CONNECT = True
  (==) (OtherMethod a) (OtherMethod b) = a == b
  (==) _ _ = False

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
record Request m h (f : m -> Type -> Type) a where
  constructor MkRequest
  method : m
  headers : h
  body : f method a

public export
simpleBody : { m : Type } -> m -> Type -> Type
simpleBody m a = a

export
Functor (Request m h TyTTP.Request.simpleBody) where
  map f req = record { body $= f } req

public export
httpBodyOf : { monad : Type -> Type } -> { error : Type } -> Method -> Type -> Type
httpBodyOf OPTIONS _ = ()
httpBodyOf GET _ = ()
httpBodyOf HEAD _ = ()
httpBodyOf POST a = Publisher monad error a
httpBodyOf PUT a = Publisher monad error a
httpBodyOf DELETE _ = ()
httpBodyOf TRACE _ = ()
httpBodyOf CONNECT _ = ()
httpBodyOf (OtherMethod _) a = Publisher monad error a

export
mkHttpRequest : { monad : Type -> Type } -> { error : Type } -> (m : Method) -> h -> httpBodyOf { monad } { error } m a -> Request Method h (httpBodyOf { monad } { error }) a
mkHttpRequest m h a = MkRequest m h a

export
mkHttpRequestBody : { monad : Type -> Type } -> { error : Type } -> (m : Method) -> Lazy (Publisher monad error b) -> httpBodyOf {monad} {error} m b
mkHttpRequestBody OPTIONS _ = ()
mkHttpRequestBody GET _ = ()
mkHttpRequestBody HEAD _ = ()
mkHttpRequestBody POST a = a
mkHttpRequestBody PUT a = a
mkHttpRequestBody DELETE _ = ()
mkHttpRequestBody TRACE _ = ()
mkHttpRequestBody CONNECT _ = ()
mkHttpRequestBody (OtherMethod _) a = a


