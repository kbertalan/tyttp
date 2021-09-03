module Main

namespace Request

  export
  record Request a where
    constructor MkRequest
    body : a

  export
  mkRequest : a -> Request a
  mkRequest = MkRequest

namespace Response

  public export
  data Status = OK | Moved | BadRequest | InternalError

  export
  record Response a where
    constructor MkResponse
    status : Status 
    body : a

namespace Step

  export
  record Step (a : Type) (b : Type) where
    constructor MkStep
    request : Request a
    response : Response b

namespace Handler

  public export
  Handler : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
  Handler m a b c d = Step a b -> m (Step c d)

  export
  run : Functor m => Handler m a b c d -> Step a b -> m (Step c d)
  run f step = f step


hId : Applicative m => Handler m a b a b
hId = pure . id

hEcho : Applicative m => Handler m a b a a
hEcho (MkStep request response) = pure $ MkStep request $ record { body = request.body } response

hMapRequest : Applicative m => (i -> o) -> Handler m i a o a
hMapRequest f (MkStep request response) = pure $ MkStep (MkRequest $ f request.body) response

hMapResponse : Applicative m => (i -> o) -> Handler m a i a o
hMapResponse f (MkStep request response) = pure $ MkStep request $ record { body $= f } response

hConstRequest : Applicative m => cnst -> Handler m a b cnst b
hConstRequest x = hMapRequest $ const x

hConstResponse : Applicative m => cnst -> Handler m a b a cnst
hConstResponse x = hMapResponse $ const x

main : IO ()
main = do
  let req = mkRequest "request"
      res = MkResponse OK ""
      handlers =
        [ hId
        , hEcho
        , hMapRequest (<+> "-appended") >=> hEcho
        , hEcho >=> hMapResponse (<+> "-response")
        , hConstRequest "const-request" >=> hEcho
        , hEcho >=> hConstResponse "const-response"
        ]
  for_ handlers $ \handler => do
      result <- run handler $ MkStep req res
      printLn result.response.body

