module Handler

namespace Request

  public export
  record Request h a where
    constructor MkRequest
    headers : h
    body : a

  export
  Functor (Request h) where
    map f req = record { body $= f } req

namespace Response

  public export
  data Status = OK | Moved | BadRequest | InternalError

  public export
  record Response h a where
    constructor MkResponse
    status : Status 
    headers : h
    body : a

  export
  Functor (Response h) where
    map f res = record { body $= f } res

namespace Step

  public export
  record Step h1 h2 (a : Type) (b : Type) where
    constructor MkStep
    request : Request h1 a
    response : Response h2 b

  export
  Bifunctor (Step h1 h2) where
    bimap f g step = record { request $= map f, response $= map g } step


public export
Handler : (Type -> Type) -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type
Handler m h1 h2 a b h3 h4 c d = Step h1 h2 a b -> m (Step h3 h4 c d)

