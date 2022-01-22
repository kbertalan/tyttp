module TyTTP.Adapter.Node.URI

import Control.Monad.Maybe
import Node.URI
import TyTTP

export
decodeUri : Alternative m
  => (
    Step me String h1 s h2 a b
    -> m $ Step me' String h1' s' h2' a' b'
  )
  -> Step me String h1 s h2 a b
  -> m $ Step me' String h1' s' h2' a' b'
decodeUri handler step = case Node.URI.decodeURI step.request.url of
  Right str => handler $ { request.url := str } step
  Left _ => empty

export
decodeUri' : Monad m
  => (
    Step me String h1 s h2 a b
    -> m $ Step me' String h1' s' h2' a' b'
  )
  -> (
    Step me String h1 s h2 a b
    -> MaybeT m $ Step me' String h1' s' h2' a' b'
  )
  -> Step me String h1 s h2 a b
  -> m $ Step me' String h1' s' h2' a' b'
decodeUri' defHandler handler step = do
  Just result <- runMaybeT $ decodeUri handler step
    | Nothing => defHandler step
  pure result
