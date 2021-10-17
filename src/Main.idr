module Main

import Data.Buffer
import Data.List
import Data.String
import Control.Monad.Either
import Node
import Node.Error
import Node.FS
import Node.FS.Stats
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
  = FileReadError FileError
  | NotAFile Resource
  | OnlyGet

StaticRequest = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()

StaticResponse = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)

hStatic : String -> StaticRequest -> IO StaticResponse
hStatic folder step = eitherT returnError returnSuccess $ do
    let resource = step.request.path
        file = "\{folder}\{resource}"

    GET <- pure $ step.request.method
      | _ => throwError OnlyGet

    fs <- liftIO FS.require
    stats <- liftIO $ stat file
    True <- pure $ stats.isFile
      | False => throwError $ NotAFile resource

    pure $ MkPublisher $ \s => s.onSucceded ()

  where
    returnSuccess : Publisher IO NodeError Buffer -> IO StaticResponse
    returnSuccess result = do
      pure $ record { response.body = result } step

    sendError : Status -> String -> IO StaticResponse
    sendError status str = do
      let buffer = fromString str
          publisher : Publisher IO NodeError Buffer = MkPublisher $ \s => s.onNext buffer >>= s.onSucceded

      pure $ record { response.status = status, response.body = publisher } step

    returnError : FileServingError -> IO StaticResponse
    returnError code = case code of
      FileReadError e => sendError INTERNAL_SERVER_ERROR $ "File error: " <+> show e
      NotAFile s => sendError NOT_FOUND $ "Could not found file: " <+> s
      OnlyGet => sendError BAD_REQUEST $ "Only GET method is supported"

main : IO ()
main = eitherT putStrLn pure $ do
  Just cwd <- currentDir
    | Nothing => throwError "No current directory"

  let folder = "\{cwd}/tmp"
  unless !(exists folder) $ do
    Right () <- createDir folder
      | Left _ => throwError "Cannot create directory \{folder}"
    pure ()

  let resource = "file.json"
      file = "\{folder}/\{resource}"
      content = trim """
      { "some": "text", "or": -1 }
    """

  Right _ <- writeFile file content
    | Left e => throwError $ show e

  http <- liftIO require
  server <- liftIO $ HTTP.listen $ hStatic folder

  ignore $ liftIO $ http.get "http://localhost:3000/\{resource}" $ \res => do
    onData res putStrLn
    server.close

