module Main

namespace Request

  export
  record Request a where
    constructor MkRequest
    body : a

  export
  mkRequest : a -> Request a
  mkRequest = MkRequest

  export
  Functor Request where
    map f req = record { body $= f } req

namespace Response

  public export
  data Status = OK | Moved | BadRequest | InternalError

  export
  record Response a where
    constructor MkResponse
    status : Status 
    body : a

  export
  Functor Response where
    map f res = record { body $= f } res

namespace Step

  export
  record Step (a : Type) (b : Type) where
    constructor MkStep
    request : Request a
    response : Response b

  export
  Bifunctor Step where
    bimap f g step = record { request $= map f, response $= map g } step

namespace Handler

  public export
  Handler : (Type -> Type) -> Type -> Type -> Type -> Type -> Type
  Handler m a b c d = Step a b -> m (Step c d)

hId : Applicative m => Handler m a b a b
hId = pure . id

hEcho : Applicative m => Handler m a b a a
hEcho step = pure $ mapSnd (const step.request.body) step

hMapRequest : Applicative m => (i -> o) -> Handler m i a o a
hMapRequest f step = pure $ mapFst f step

hMapResponse : Applicative m => (i -> o) -> Handler m a i a o
hMapResponse f step = pure $ mapSnd f step

hConstRequest : Applicative m => c-> Handler m a b c b
hConstRequest x = hMapRequest $ const x

hConstResponse : Applicative m => c -> Handler m a b a c
hConstResponse x = hMapResponse $ const x

main : IO ()
main = do
  let req = mkRequest "request"
      res = MkResponse OK ""
      initialStep = MkStep req res
      handlers =
        [ hId
        , hEcho
        , hMapRequest (<+> "-appended") >=> hEcho
        , hEcho >=> hMapResponse (<+> "-response")
        , hConstRequest "const-request" >=> hEcho
        , hEcho >=> hConstResponse "const-response"
        ]
  for_ handlers $ \handler => do
      result <- handler initialStep
      putStrLn result.response.body

  putStrLn "\nfoldlM\n"

  let collection = [ hConstRequest "init", hMapRequest (<+> "-req"), hEcho, hMapResponse (<+> "-res") ] 
  step <- foldlM (flip ($)) initialStep collection

  putStrLn step.response.body

