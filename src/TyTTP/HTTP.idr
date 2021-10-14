module TyTTP.HTTP

import TyTTP
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
parseMethod : String -> Method
parseMethod str = case str of
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
selectBodyByMethod : Method -> a -> a -> a
selectBodyByMethod m withoutBody withBody = case m of
  OPTIONS => withoutBody
  GET => withoutBody
  HEAD => withoutBody
  POST => withBody
  PUT => withBody
  DELETE => withoutBody
  TRACE => withoutBody
  CONNECT => withoutBody
  OtherMethod _ => withBody


public export
bodyOf : { monad : Type -> Type } -> { error : Type } -> Method -> Type -> Type
bodyOf m a = selectBodyByMethod m () $ Publisher monad error a

public export
HttpRequest : { auto monad : Type -> Type } -> { auto error : Type} -> Type -> Type -> Type -> Type
HttpRequest p h a = Request Method p h (bodyOf { monad } { error }) a

export
mkRequest : { monad : Type -> Type } -> { error : Type } -> (m : Method) -> p -> h -> bodyOf { monad } { error } m a -> HttpRequest { monad } { error } p h a
mkRequest m p h a = MkRequest m p h a

export
mkRequestBody : { monad : Type -> Type } -> { error : Type } -> (m : Method) -> Lazy (Publisher monad error b) -> bodyOf {monad} {error} m b
mkRequestBody m x = case m of
  OPTIONS => ()
  GET => ()
  HEAD => ()
  POST => x
  PUT => x
  DELETE => ()
  TRACE => ()
  CONNECT => ()
  (OtherMethod _) => x

