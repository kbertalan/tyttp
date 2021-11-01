module TyTTP.Adapter.Node.Static

import Data.Buffer
import Control.Monad.Either
import Node.Error
import Node.FS
import Node.FS.Stats
import Node.FS.Stream
import TyTTP.Adapter.Node.HTTP
import TyTTP.HTTP
import TyTTP.Path
import TyTTP.URL

public export
Resource : Type
Resource = String

public export
data FileServingError
  = StatError NodeError
  | NotAFile Resource

public export
StaticRequest : Type -> Type
StaticRequest url = Step Method url StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()

public export
StaticResponse : Type -> Type
StaticResponse url = Step Method url StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)

record StaticSuccesResult where
  constructor MkStaticSuccessResult
  size : Int
  stream : Publisher IO NodeError Buffer

export
hStatic : (folder : String)
  -> (returnError : FileServingError
    -> StaticRequest (URL a Path s)
    -> IO $ StaticResponse (URL a Path s)
  )
  -> (step : StaticRequest (URL a Path s))
  -> IO $ StaticResponse (URL a Path s)
hStatic folder returnError step = eitherT (flip returnError step) returnSuccess $ do
    let resource = step.request.url.path.rest
        file = "\{folder}\{resource}"

    fs <- FS.require
    Right stats <- stat file
      | Left e => throwError $ case e.code of
         SystemError ENOENT => NotAFile resource
         _ => StatError e

    True <- pure $ stats.isFile
      | _ => throwError $ NotAFile resource

    False <- pure $ stats.isDirectory
      | _ => throwError $ NotAFile resource

    readStream <- createReadStream file

    pure $ MkStaticSuccessResult
            { size = stats.size }
            { stream = MkPublisher $ \s => do
                readStream.onData  s.onNext
                readStream.onEnd   s.onSucceded
                readStream.onError s.onFailed
            }

  where
    returnSuccess : StaticSuccesResult -> IO $ StaticResponse (URL a Path s)
    returnSuccess result = do
      let hs = ("Content-Length", show $ result.size) :: headers step.response 
      pure $ record { response.status = OK, response.headers = hs, response.body = result.stream } step

