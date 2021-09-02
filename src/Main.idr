module Main

namespace Request

  export
  record Request a where
    constructor MkRequest
    body: a

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
    request: Request a
    response: Response b

namespace Handler

  public export
  Handler : Type -> Type -> Type -> Type -> Type
  Handler a b c d = Step a b -> Step c d

  export
  run : Handler a b c d -> Step a b -> Step c d
  run f step = f step


hId : Handler a b a b
hId = id

hEcho : Handler a b a a
hEcho (MkStep request response) = MkStep request $ record { body = request.body } response

hMapRequest : (i -> o) -> Handler i a o a
hMapRequest f (MkStep request response) = MkStep (MkRequest $ f request.body) response

hMapResponse : (i -> o) -> Handler a i a o
hMapResponse f (MkStep request response) = MkStep request $ record { body $= f } response
hConstRequest : cnst -> Handler a b cnst b
hConstRequest x = hMapRequest $ const x

hConstResponse : cnst -> Handler a b a cnst
hConstResponse x = hMapResponse $ const x


main : IO ()
main = do
  let req = mkRequest "request"
      res = MkResponse OK ""
      handlers = [
        hId
        , hEcho
        , hEcho . hMapRequest (<+> "-appended")
        , hMapResponse (<+> "-response") . hEcho
        , hEcho . hConstRequest "const-request"
        , hConstResponse "const-response" . hEcho
        ]
  for_ handlers $ \handler => printLn $ body $ response $ run handler $ MkStep req res

