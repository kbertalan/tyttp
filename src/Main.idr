module Main

import Data.Buffer
import Data.List
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
import Node
import Node.Buffer
import Node.Error
import Node.FS
import Node.FS.Stats
import Node.FS.Stream
import Node.HTTP.Server
import System
import System.Directory
import TyTTP
import TyTTP.Adapter.Node.HTTP
import TyTTP.HTTP
import TyTTP.HTTP.Routing as R
import TyTTP.Path
import TyTTP.Routing as R

Resource : Type
Resource = String

data FileServingError
  = StatError NodeError
  | NotAFile Resource

StaticRequest : Type -> Type
StaticRequest url = Step Method url StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()

StaticResponse : Type -> Type
StaticResponse url = Step Method url StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)

record StaticSuccesResult where
  constructor MkStaticSuccessResult
  size : Int
  stream : Publisher IO NodeError Buffer

sendError : Status -> String -> StaticRequest a -> IO $ StaticResponse a
sendError status str step = do
  let buffer = fromString str
      publisher : Publisher IO NodeError Buffer = singleton buffer

  size <- rawSize buffer

  pure $ record 
    { response.headers = 
      [ ("Content-Type", "text/plain")
      , ("Content-Length", show size)
      ]
    , response.status = status
    , response.body = publisher
    } step

hStatic : String -> StaticRequest Path -> IO $ StaticResponse Path
hStatic folder step = eitherT returnError returnSuccess $ do
    let resource = step.request.url.rest
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
    returnSuccess : StaticSuccesResult -> IO $ StaticResponse Path
    returnSuccess result = do
      let hs = ("Content-Length", show $ result.size) :: headers step.response 
      pure $ record { response.status = OK, response.headers = hs, response.body = result.stream } step

    returnError : FileServingError -> IO $ StaticResponse Path
    returnError code = case code of
      StatError e => sendError INTERNAL_SERVER_ERROR ("File error: " <+> e.message) step
      NotAFile s => sendError NOT_FOUND ("Could not found file: " <+> s) step

hRouting : String -> StaticRequest String -> IO $ StaticResponse String
hRouting folder step = do
    let error = delay $ sendError NOT_FOUND "Resource could not be found" step
    R.routesWithDefault error
      [ R.get $ pattern "/static/*" :> hStatic folder
      , R.post :> sendError INTERNAL_SERVER_ERROR "This is just an example"
      ] step

main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- ((head' <=< tail' <=< tail') <$> getArgs) <|> currentDir
    | Nothing => throwError "Folder could not be located"

  putStrLn "Starting static server in folder: \{folder}"

  http <- HTTP.require
  ignore $ HTTP.listen $ hRouting folder

