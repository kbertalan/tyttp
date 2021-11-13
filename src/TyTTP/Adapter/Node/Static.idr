module TyTTP.Adapter.Node.Static

import Data.Buffer
import Data.Maybe
import Data.Mime.Apache
import Data.List
import Control.Monad.Either
import Node.Error
import Node.FS
import Node.FS.Stats
import Node.FS.Stream
import TyTTP.Adapter.Node.HTTP
import TyTTP.HTTP
import TyTTP.URL
import TyTTP.URL.Path

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
  mime : Mime

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
    Right stats <- stat_sync StatsInt file
      | Left e => throwError $ case e.code of
         SystemError ENOENT => NotAFile resource
         _ => StatError e

    True <- pure $ stats.isFile
      | _ => throwError $ NotAFile resource

    False <- pure $ stats.isDirectory
      | _ => throwError $ NotAFile resource

    readStream <- createReadStream file

    pure $ MkStaticSuccessResult
            { size = Stats.size stats
            , stream = MkPublisher $ \s => do
                readStream.onData  s.onNext
                readStream.onEnd   s.onSucceded
                readStream.onError s.onFailed
            , mime = mimeOf file
            }

  where
    returnSuccess : StaticSuccesResult -> IO $ StaticResponse (URL a Path s)
    returnSuccess result = do
      let hs = [ ("Content-Length", show $ result.size)
               , ("Content-Type", show $ result.mime)
               ]
      pure $ { response.status := OK
             , response.headers := hs
             , response.body := result.stream } step

    extensionOf' : (ext: List Char) -> (file: List Char) -> (dot: Bool) -> Maybe (List Char)
    extensionOf' ext ('.' :: xs) _ = extensionOf' xs xs True
    extensionOf' ext (x :: xs) dot = extensionOf' ext xs dot
    extensionOf' ext [] True = Just ext
    extensionOf' ext [] False = Nothing

    extensionOf : String -> Maybe String
    extensionOf file = let l = unpack file in
                           map pack $ extensionOf' l l False

    mimeOf : String -> Mime
    mimeOf file =
      fromMaybe TEXT_PLAIN $
        flip lookup extensions =<< extensionOf file

