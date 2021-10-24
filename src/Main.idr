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
import TyTTP.Routing as R

Resource : Type
Resource = String

data FileServingError
  = StatError NodeError
  | NotAFile Resource

StaticRequest = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()

StaticResponse = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)

record StaticSuccesResult where
  constructor MkStaticSuccessResult
  size : Int
  stream : Publisher IO NodeError Buffer

sendError : Status -> String -> StaticRequest -> IO StaticResponse
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

hStatic : String -> StaticRequest -> IO StaticResponse
hStatic folder step = eitherT returnError returnSuccess $ do
    let resource = step.request.url
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
    returnSuccess : StaticSuccesResult -> IO StaticResponse
    returnSuccess result = do
      let hs = ("Content-Length", show $ result.size) :: headers step.response 
      pure $ record { response.status = OK, response.headers = hs, response.body = result.stream } step

    returnError : FileServingError -> IO StaticResponse
    returnError code = case code of
      StatError e => sendError INTERNAL_SERVER_ERROR ("File error: " <+> e.message) step
      NotAFile s => sendError NOT_FOUND ("Could not found file: " <+> s) step

hStatic2 : String -> StaticRequest -> IO StaticResponse
hStatic2 folder step = do
    let error = delay $ sendError BAD_REQUEST "Invalid method" step
    R.routesWithDefault error
      [ R.get $ \s => lift $ hStatic folder s
      , R.post $ \s => lift $ sendError INTERNAL_SERVER_ERROR "Should not run this" s
      ] step

main : IO ()
main = eitherT putStrLn pure $ do
  Just folder <- ((head' <=< tail' <=< tail') <$> getArgs) <|> currentDir
    | Nothing => throwError "Folder could not be located"

  putStrLn "Starting static server in folder: \{folder}"

  http <- HTTP.require
  ignore $ HTTP.listen $ hStatic2 folder

