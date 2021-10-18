module Main

import Data.Buffer
import Data.List
import Data.String
import Control.Monad.Either
import Node
import Node.Error
import Node.FS
import Node.FS.Stats
import Node.FS.Stream
import Node.HTTP.Client
import Node.HTTP.Server
import System.Directory
import System.File
import TyTTP
import TyTTP.HTTP
import TyTTP.Adapter.Node.HTTP

Resource : Type
Resource = String

data FileServingError
  = StatError NodeError
  | NotAFile Resource
  | OnlyGet

StaticRequest = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()

StaticResponse = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)

record StaticSuccesResult where
  constructor MkStaticSuccessResult
  size : Int
  stream : Publisher IO NodeError Buffer

hStatic : String -> StaticRequest -> IO StaticResponse
hStatic folder step = eitherT returnError returnSuccess $ do
    let resource = step.request.path
        file = "\{folder}\{resource}"

    GET <- pure $ step.request.method
      | _ => throwError OnlyGet

    fs <- liftIO FS.require
    Right stats <- liftIO $ stat file
      | Left e => throwError $ StatError e

    True <- pure $ stats.isFile
      | _ => throwError $ NotAFile resource

    False <- pure $ stats.isDirectory
      | _ => throwError $ NotAFile resource

    readStream <- liftIO $ createReadStream file

    pure $ MkStaticSuccessResult
            { size = stats.size }
            { stream = MkPublisher $ \s => do
                readStream.onData  s.onNext
                readStream.onEnd   s.onSucceded
                readStream.onError s.onFailed
            }

  where
    returnSuccess : StaticSuccesResult -> IO StaticResponse
    returnSuccess result = do
      let hs = ("Content-Length", show $ result.size) :: headers step.response 
      pure $ record { response.status = OK, response.headers = hs, response.body = result.stream } step

    sendError : Status -> String -> IO StaticResponse
    sendError status str = do
      let buffer = fromString str
          publisher : Publisher IO NodeError Buffer = MkPublisher $ \s => s.onNext buffer >>= s.onSucceded

      pure $ record { response.status = status, response.body = publisher } step

    returnError : FileServingError -> IO StaticResponse
    returnError code = case code of
      StatError e => sendError INTERNAL_SERVER_ERROR $ "File error: " <+> e.message
      NotAFile s => sendError NOT_FOUND $ "Could not found file: " <+> s
      OnlyGet => sendError BAD_REQUEST $ "Only GET method is supported"

main : IO ()
main = eitherT putStrLn pure $ do
  Just cwd <- currentDir
    | Nothing => throwError "No current directory"

  http <- liftIO HTTP.require
  ignore $ liftIO $ HTTP.listen $ hStatic cwd

