module TyTTP.URL.Simple

import public Control.Monad.Either
import Data.List
import Data.String
import TyTTP
import TyTTP.URL.Definition

public export
SimpleURL : Type
SimpleURL = URL String String String

public export
data URLParserError
  = EmptyString
  | MissingAuthorityOrPath

export -- visible for testing
parse : String -> Either URLParserError SimpleURL
parse str = scheme (unpack $ trim str) $ MkURL Nothing Nothing "" ""
  where
    search : List Char -> SimpleURL -> Either URLParserError SimpleURL
    search [] url = Right url
    search xs url = Right $ { search := pack xs } url

    path : List Char -> SimpleURL -> Either URLParserError SimpleURL
    path [] url = Right $ { path := "/" } url
    path xs url =
      let (p, rest) = List.break (== '?') xs
      in search rest $ { path := pack p } url

    authority : List Char -> SimpleURL -> Either URLParserError SimpleURL
    authority ('/' :: '/' :: xs) url =
      let (auth, rest) = List.break (== '/') xs
      in path rest $ { authority := Just $ pack auth } url
    authority [] _ = Left MissingAuthorityOrPath
    authority x url = path x url

    scheme : List Char -> SimpleURL -> Either URLParserError SimpleURL
    scheme ('h' :: 't' :: 't' :: 'p' :: xs) url = 
      case xs of
         ('s' :: ':' :: ys) => authority ys $ { scheme := Just HTTPS } url
         (':' :: ys) => authority ys $ { scheme := Just HTTP } url
         xs => path ('h' :: 't' :: 't' :: 'p' :: xs) url
    scheme [] _ = Left EmptyString
    scheme xs url = authority xs url

export
parseUrl : MonadError URLParserError m
  => (
    Context me SimpleURL v h1 s h2 a b
    -> m $ Context me' SimpleURL v' h1' s' h2' a' b'
  )
  -> Context me String v h1 s h2 a b
  -> m $ Context me' String v' h1' s' h2' a' b'
parseUrl handler ctx = case parse ctx.request.url of
  Right u => do
    result <- handler $ { request.url := u } ctx
    pure $ { request.url := ctx.request.url } result
  Left err => throwError err

export
parseUrl' : Monad m
  => (
    URLParserError
    -> Context me String v h1 s h2 a b
    -> m $ Context me' String v' h1' s' h2' a' b'
  )
  -> (
    Context me SimpleURL v h1 s h2 a b
    -> EitherT URLParserError m $ Context me' SimpleURL v' h1' s' h2' a' b'
  )
  -> Context me String v h1 s h2 a b
  -> m $ Context me' String v' h1' s' h2' a' b'
parseUrl' errHandler handler ctx = do
  Right result <- runEitherT $ Simple.parseUrl handler ctx
    | Left err => errHandler err ctx
  pure result


