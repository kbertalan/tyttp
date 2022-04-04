module TyTTP.HTTP.Routing

import Data.Mime.Apache
import Data.String

import TyTTP
import TyTTP.HTTP

namespace Method

  methodRouter : Alternative m
    => Method 
    -> (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  methodRouter m handler ctx =
    if ctx.request.method == m 
    then handler ctx
    else empty

  export
  options : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  options = methodRouter OPTIONS

  export
  get : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  get = methodRouter GET

  export
  head : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  head = methodRouter HEAD

  export
  post : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  post = methodRouter POST

  export
  put : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  put = methodRouter PUT

  export
  delete : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  delete = methodRouter DELETE

  export
  trace : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  trace = methodRouter TRACE

  export
  connect : Alternative m
    => (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  connect = methodRouter CONNECT

  export
  other : Alternative m
    => String
    -> (
      Context Method u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context Method u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
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
      Context me u v h1 s h2 a b
      -> m $ Context me' p' v' h1' s' h2' a' b'
    )
    -> Context me u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  contentType mime handler ctx =
    case stringMatchesMime mime <$> getContentType ctx.request.headers of
      Just True => handler ctx
      _ => empty

  export
  json : Alternative m
    => HasContentType h1
    => (
      Context me u v h1 s h2 a b
      -> m $ Context me' p' v' h1 s' h2' a' b'
    )
    -> Context me u v h1 s h2 a b
    -> m $ Context me' p' v' h1 s' h2' a' b'
  json = contentType APPLICATION_JSON

  export
  text : Alternative m
    => HasContentType h1
    => (
      Context me u v h1 s h2 a b
      -> m $ Context me' p' v' h1 s' h2' a' b'
    )
    -> Context me u v h1 s h2 a b
    -> m $ Context me' p' v' h1 s' h2' a' b'
  text = contentType TEXT_PLAIN

  export
  binary : Alternative m
    => HasContentType h1
    => (
      Context me u v h1 s h2 a b
      -> m $ Context me' p' v' h1 s' h2' a' b'
    )
    -> Context me u v h1 s h2 a b
    -> m $ Context me' p' v' h1 s' h2' a' b'
  binary = contentType APPLICATION_OCTET_STREAM
