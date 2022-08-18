module TyTTP.Adapter.Node.Static

import Data.Buffer
import Data.Maybe
import Data.Mime.Apache
import Data.List
import Control.Monad.Either
import Node.Error
import Node.FS
import TyTTP.Adapter.Node.Error
import TyTTP.Adapter.Node.HTTP
import TyTTP.HTTP
import TyTTP.URL
import TyTTP.URL.Path

public export
Resource : Type
Resource = String

public export
data FileServingError : Type where
  StatError : Error e => e -> FileServingError
  NotAFile : Resource -> FileServingError

public export
StaticRequest : Type -> Type
StaticRequest url = Context Method url Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) ()

public export
StaticResponse : Type -> Type
StaticResponse url = Context Method url Version StringHeaders Status StringHeaders (Publisher IO Error Buffer) (Publisher IO Error Buffer)

record StaticSuccesResult where
  constructor MkStaticSuccessResult
  size : Int
  stream : Publisher IO Error Buffer
  mime : Mime

export
hStatic : HasIO io
  => (folder : String)
  -> (returnError : FileServingError
    -> StaticRequest (URL a Path s)
    -> io $ StaticResponse (URL a Path s)
  )
  -> (ctx : StaticRequest (URL a Path s))
  -> io $ StaticResponse (URL a Path s)
hStatic folder returnError ctx = eitherT (flip returnError ctx) returnSuccess $ do
    let resource = ctx.request.url.path.rest
        file = "\{folder}\{resource}"

    fs <- FS.require
    Right stats <- fs.statSync StatsInt file
      | Left e => throwError $ case code e of
         SystemError ENOENT => NotAFile resource
         _ => StatError e

    True <- pure $ stats.isFile
      | _ => throwError $ NotAFile resource

    False <- pure $ stats.isDirectory
      | _ => throwError $ NotAFile resource

    readStream <- fs.createReadStream file

    pure $ MkStaticSuccessResult
            { size = Stats.size stats
            , stream = MkPublisher $ \s => do
                readStream.onData  s.onNext
                readStream.onEnd   $ s.onSucceded ()
                readStream.onError s.onFailed
            , mime = mimeOf file
            }

  where
    returnSuccess : StaticSuccesResult -> io $ StaticResponse (URL a Path s)
    returnSuccess result = do
      let hs = [ ("Content-Length", show $ result.size)
               , ("Content-Type", show $ result.mime)
               ]
      pure $ { response.status := OK
             , response.headers := hs
             , response.body := result.stream } ctx 

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

