module TyTTP.Core.Routing

import Control.Monad.Maybe
import TyTTP.Core.Context

export
routes : Alternative m
  => List (
    Context me u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  )
  -> Context me u v h1 s h2 a b
  -> m $ Context me' p' v' h1' s' h2' a' b'
routes handlers ctx = choiceMap ($ ctx) handlers

export
routes' : Monad m
  => (
    Context me u v h1 s h2 a b
    -> m $ Context me' p' v' h1' s' h2' a' b'
  )
  -> List (
    Context me u v h1 s h2 a b
    -> MaybeT m $ Context me' p' v' h1' s' h2' a' b'
  )
  -> Context me u v h1 s h2 a b
  -> m $ Context me' p' v' h1' s' h2' a' b'
routes' def handlers ctx = do
  Just result <- runMaybeT $ routes handlers ctx
    | Nothing => def ctx
  pure result

