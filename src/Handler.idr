module Handler

namespace Request

  public export
  record Request a where
    constructor MkRequest
    body : a

  export
  Functor Request where
    map f req = record { body $= f } req

namespace Response

  public export
  data Status = OK | Moved | BadRequest | InternalError

  public export
  record Response a where
    constructor MkResponse
    status : Status 
    body : a

  export
  Functor Response where
    map f res = record { body $= f } res

namespace Step

  public export
  record Step (a : Type) (b : Type) where
    constructor MkStep
    request : Request a
    response : Response b

  export
  Bifunctor Step where
    bimap f g step = record { request $= map f, response $= map g } step


public export
Handler : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
Handler m a b c d = Step a b -> m (Step c d)

