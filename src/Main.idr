module Main

namespace Request

  export
  data Request a = MkRequest a

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

  export
  def : Monoid a => Response a
  def = MkResponse OK neutral


namespace Handler

  export
  data Handler : Type -> Type -> Type where
    MkHandler : (Request a -> Response b -> Response b) -> Handler a b

  export
  mkHandler : (Request a -> Response b -> Response b) -> Handler a b
  mkHandler = MkHandler

  export
  runHandler : Handler a b -> Request a -> Response b -> Response b
  runHandler (MkHandler f) req res = f req res

  export
  chain : Handler a b -> Handler a b -> Handler a b
  chain (MkHandler f) (MkHandler g) = MkHandler $ \req, res =>
    f req (g req res)


hId : Handler a b
hId = mkHandler $ \req, res => res

hOverwrite : b -> Handler a b
hOverwrite msg = mkHandler $ \req, res => record { body = msg } res

hAppender : Semigroup b => b -> Handler a b
hAppender msg = mkHandler $ \req, res => record { body $= (<+> msg) } res

main : IO ()
main = do
  let req = mkRequest "request"
      handlers = [
        hId,
        hAppender [1],
        hOverwrite [2],
        hOverwrite [2] `chain` hAppender [3],
        hAppender [3] `chain` hOverwrite [2]
        ]
  for_ handlers $ \h => printLn $ body $ runHandler h req def
