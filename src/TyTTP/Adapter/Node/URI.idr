module TyTTP.Adapter.Node.URI

import Control.Monad.Maybe
import Node.URI
import TyTTP

export
decodeUri : Alternative m
  => (
    Context me String h1 s h2 a b
    -> m $ Context me' String h1' s' h2' a' b'
  )
  -> Context me String h1 s h2 a b
  -> m $ Context me' String h1' s' h2' a' b'
decodeUri handler ctx = case Node.URI.decodeURI ctx.request.url of
  Right str => handler $ { request.url := str } ctx
  Left _ => empty

export
decodeUri' : Monad m
  => (
    Context me String h1 s h2 a b
    -> m $ Context me' String h1' s' h2' a' b'
  )
  -> (
    Context me String h1 s h2 a b
    -> MaybeT m $ Context me' String h1' s' h2' a' b'
  )
  -> Context me String h1 s h2 a b
  -> m $ Context me' String h1' s' h2' a' b'
decodeUri' defHandler handler ctx = do
  Just result <- runMaybeT $ decodeUri handler ctx
    | Nothing => defHandler ctx
  pure result
