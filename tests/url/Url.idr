module Url

import Control.Monad.Either
import TyTTP.URL

main : IO ()
main = eitherT putStrLn pure $ do
  Left EmptyString <- pure $ Simple.parse ""
    | _ => throwError "empty url is not matched"

  Left EmptyString <- pure $ Simple.parse " \t\r\n"
    | _ => throwError "blank url is not matched"

  Left MissingAuthorityOrPath <- pure $ Simple.parse "http:"
    | _ => throwError "http scheme not recognized"

  Right (MkURL (Just HTTP) Nothing "something" "") <- pure $ Simple.parse "http:something"
    | _ => throwError "http scheme not recognized"

  Right (MkURL (Just HTTPS) Nothing "som" "") <- pure $ Simple.parse " \thttps:som"
    | _ => throwError "https scheme with whitespace not recognized"

  Right (MkURL Nothing (Just "something") "/" "") <- pure $ Simple.parse "//something"
    | _ => throwError "simple authority not recognized"

  Right (MkURL (Just HTTP) (Just "user:passwd@something") "/path/like" "?query=string") <- pure $ Simple.parse "http://user:passwd@something/path/like?query=string"
    | _ => throwError "simple authority not recognized"

  pure ()


