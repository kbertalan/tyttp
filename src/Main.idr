module Main

import Data.Buffer
import Data.List
import Data.String
import Control.Monad.Either
import Control.Monad.Maybe
import Control.Monad.Trans
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

StaticRequest = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer ()

StaticResponse = Step Method String StringHeaders (TyTTP.HTTP.bodyOf { monad = IO } { error = NodeError }) Status StringHeaders Buffer (Publisher IO NodeError Buffer)

record StaticSuccesResult where
  constructor MkStaticSuccessResult
  size : Int
  stream : Publisher IO NodeError Buffer

sendError : Status -> String -> StaticRequest -> IO StaticResponse
sendError status str step = do
  let buffer = fromString str
      publisher : Publisher IO NodeError Buffer = MkPublisher $ \s => s.onNext buffer >>= s.onSucceded

  pure $ record { response.headers = [("Content-Type", "text/plain")], response.status = status, response.body = publisher } step

hStatic : String -> StaticRequest -> IO StaticResponse
hStatic folder step = eitherT returnError returnSuccess $ do
    let resource = step.request.path
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
    let error = sendError BAD_REQUEST "Invalid method" step
    fromMaybeT error $ routes
      [ get $ \s => lift $ hStatic folder s
      , post $ \s => lift $ sendError INTERNAL_SERVER_ERROR "Should not run this" s
      ] step
  where
    routes : Alternative m
      => List (
        Step me p h1 fn s h2 a b
        -> m $ Step me' p' h1' fn' s' h2' a' b'
      )
      -> Step me p h1 fn s h2 a b
      -> m $ Step me' p' h1' fn' s' h2' a' b'
    routes handlers step = choiceMap ($ step) handlers

    get : Alternative m
      => (
        Step Method p h1 fn s h2 a b
        -> m $ Step me' p' h1' fn' s' h2' a' b'
      )
      -> Step Method p h1 fn s h2 a b
      -> m $ Step me' p' h1' fn' s' h2' a' b'
    get handler step = case step.request.method of
      GET => handler step
      _ => empty

    post : Alternative m
      => (
        Step Method p h1 fn s h2 a b
        -> m $ Step me' p' h1' fn' s' h2' a' b'
      )
      -> Step Method p h1 fn s h2 a b
      -> m $ Step me' p' h1' fn' s' h2' a' b'
    post handler step = case step.request.method of
      POST => handler step
      _ => empty

main : IO ()
main = eitherT putStrLn pure $ do
  Just cwd <- currentDir
    | Nothing => throwError "No current directory"

  http <- HTTP.require
  ignore $ HTTP.listen $ hStatic2 cwd

