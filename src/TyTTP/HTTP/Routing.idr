module TyTTP.HTTP.Routing

import Data.Mime.Apache
import Data.String

import TyTTP
import TyTTP.HTTP

namespace Method

  methodRouter : Alternative m
    => Method 
    -> (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  methodRouter m handler step =
    if step.request.method == m 
    then handler step
    else empty

  export
  options : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  options = methodRouter OPTIONS

  export
  get : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  get = methodRouter GET

  export
  head : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  head = methodRouter HEAD

  export
  post : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  post = methodRouter POST

  export
  put : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  put = methodRouter PUT

  export
  delete : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  delete = methodRouter DELETE

  export
  trace : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  trace = methodRouter TRACE

  export
  connect : Alternative m
    => (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  connect = methodRouter CONNECT

  export
  other : Alternative m
    => String
    -> (
      Step Method u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step Method u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  other str = methodRouter (OtherMethod str)

namespace ContentType

  stringMatchesMime : Mime -> String -> Bool
  stringMatchesMime mime candidate =
    let mimeString = show mime
    in isPrefixOf mimeString $ toLower candidate

  export
  contentType : Alternative m
    => HasContentType h1
    => Mime
    -> (
      Step me u h1 s h2 a b
      -> m $ Step me' p' h1' s' h2' a' b'
    )
    -> Step me u h1 s h2 a b
    -> m $ Step me' p' h1' s' h2' a' b'
  contentType mime handler step =
    case stringMatchesMime mime <$> getContentType step.request.headers of
      Just True => handler step
      _ => empty

  export
  json : Alternative m
    => HasContentType h1
    => (
      Step me u h1 s h2 a b
      -> m $ Step me' p' h1 s' h2' a' b'
    )
    -> Step me u h1 s h2 a b
    -> m $ Step me' p' h1 s' h2' a' b'
  json = contentType APPLICATION_JSON

  export
  text : Alternative m
    => HasContentType h1
    => (
      Step me u h1 s h2 a b
      -> m $ Step me' p' h1 s' h2' a' b'
    )
    -> Step me u h1 s h2 a b
    -> m $ Step me' p' h1 s' h2' a' b'
  text = contentType TEXT_PLAIN

  export
  binary : Alternative m
    => HasContentType h1
    => (
      Step me u h1 s h2 a b
      -> m $ Step me' p' h1 s' h2' a' b'
    )
    -> Step me u h1 s h2 a b
    -> m $ Step me' p' h1 s' h2' a' b'
  binary = contentType APPLICATION_OCTET_STREAM
