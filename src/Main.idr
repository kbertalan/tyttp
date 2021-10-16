module Main

import Data.Buffer
import Data.List
import Data.String
import Control.Monad.Either
import Node
import Node.Error
import Node.HTTP.Client
import Node.HTTP.Server
import System.Directory
import System.File
import TyTTP
import TyTTP.HTTP
import TyTTP.Adapter.Node.HTTP

hStatic : String 
  -> Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()
  -> IO $ Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)
hStatic folder step = eitherT respondError respondSuccess $ do
  let resource = pack $ delete '/' $ unpack step.request.path
  Right buffer <- createBufferFromFile "\{folder}/\{resource}"
    | Left e => throwError $ fromString $ show e
  pure buffer

  where
    respondError : Buffer -> IO $ Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)
    respondError buffer = pure $ record { response.status = NOT_FOUND, response.body = MkPublisher $ \s => s.onNext buffer >>= s.onSucceded } step
    respondSuccess : Buffer -> IO $ Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)
    respondSuccess buffer = pure $ record { response.status = OK, response.body = MkPublisher $ \s => s.onNext buffer >>= s.onSucceded } step

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

