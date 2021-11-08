module TyTTP.Support.Routing

import Control.Monad.Maybe
import Control.Monad.Trans
import TyTTP

export
routes : Alternative m
  => List (
    Step me u h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step me u h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
routes handlers step = choiceMap ($ step) handlers

export
routes' : Monad m
  => (
    Step me u h1 fn s h2 a b
    -> m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> List (
    Step me u h1 fn s h2 a b
    -> MaybeT m $ Step me' p' h1' fn' s' h2' a' b'
  )
  -> Step me u h1 fn s h2 a b
  -> m $ Step me' p' h1' fn' s' h2' a' b'
routes' def handlers step = do
  Just result <- runMaybeT $ routes handlers step
    | Nothing => def step
  pure result

infixr 0 :>
export
(:>) : MonadTrans t
  => Monad m
  => (f : (a -> (t m) b) -> c)
  -> (handler : a -> m b)
  -> c
(:>) f handler = f $ \a => lift $ handler a

