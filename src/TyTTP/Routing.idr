module TyTTP.Routing

import Control.Monad.Maybe
import TyTTP.Step

export
routes : Alternative m
  => List (
    Step me p h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step me p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
routes handlers step = choiceMap ($ step) handlers

export
routesWithDefault : Monad m
  => Lazy (m $ Step me' p' h1' fn' s' h2' a' b')
  -> List (
    Step me p h1 fn s h2 a b
    -> MaybeT m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step me p h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
routesWithDefault def handlers step =
  fromMaybeT def $ routes handlers step
