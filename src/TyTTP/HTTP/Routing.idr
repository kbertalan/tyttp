module TyTTP.HTTP.Routing

import public TyTTP.Routing
import TyTTP
import TyTTP.HTTP

methodRouter : Alternative m
  => Method 
  -> (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
methodRouter m handler step =
  if step.request.method == m 
  then handler step
  else empty

export
options : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
options = methodRouter OPTIONS

export
get : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
get = methodRouter GET

export
head : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
head = methodRouter HEAD

export
post : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
post = methodRouter POST

export
put : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
put = methodRouter PUT

export
delete : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
delete = methodRouter DELETE

export
trace : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
trace = methodRouter TRACE

export
connect : Alternative m
  => (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
connect = methodRouter CONNECT

export
other : Alternative m
  => String
  -> (
    Step Method p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step Method p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
other str = methodRouter (OtherMethod str)

