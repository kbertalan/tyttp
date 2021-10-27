module TyTTP.Path

import Data.Maybe
import TyTTP

public export
data Pattern : Type where
  Literal : String -> Pattern
  Param : String -> Pattern
  Rest : Pattern

Eq Pattern where
  (==) (Literal s1) (Literal s2) = s1 == s2
  (==) (Param s1) (Param s2) = s1 == s2
  (==) Rest Rest = True
  (==) _ _ = False

data ParseState
  = InLiteral (List Char)
  | InParam (List Char)
  | InRest

parse : String -> Maybe $ List Pattern
parse = map reverse . go (InLiteral []) [] . unpack
  where
    collect : List Char -> String
    collect = pack . reverse

    allowed : List Char
    allowed = map chr $ [ord '-', ord '_'] ++ [ x | x <- [ord 'a' .. ord 'z']] ++ [ x | x <- [ord 'A' .. ord 'Z']] 

    go : ParseState -> List Pattern -> List Char -> Maybe $ List Pattern
    go (InLiteral s) p ('{' :: xs) = go (InParam []) (Literal (collect s) :: p) xs
    go (InLiteral s) p ('*' :: xs) = go InRest (Literal (collect s) :: p) xs
    go (InLiteral s) p ['/'] = Just $ Literal (collect s) :: p
    go (InLiteral []) p [] = Just p
    go (InLiteral s) p [] = Just $ Literal (collect s) :: p
    go (InLiteral s) p (x :: xs) = go (InLiteral $ x :: s) p xs
    go (InParam s) p ('}' :: xs) = 
      let param = Param $ collect s
      in if elem param p || null s
      then Nothing
      else go (InLiteral []) (param :: p) xs
    go (InParam s) p [] = Nothing
    go (InParam s) p (x :: xs) =
      if elem x allowed
      then go (InParam (x :: s)) p xs
      else Nothing
    go InRest p [] = Just $ Rest :: p
    go InRest p (x :: _) = Nothing

public export
record Path where
  constructor MkPath
  raw : String
  params : List (String, String)
  rest : String
  search : String

matcher : List Pattern -> String -> Maybe Path
matcher ps s = ?t

export
pattern : Monad m 
  => Alternative m 
  => (str : String)
  -> { default (parse str) parsed : Maybe $ List Pattern}
  -> { prf : isJust parsed = True }
  -> (
    Step me Path h1 fn st h2 a b
    -> m $ Step me' Path h1' fn' st' h2' a' b'
  )
  -> Step me String h1 fn st h2 a b
  -> m $ Step me' String h1' fn' st' h2' a' b'
pattern str handler step =
  let Just p = parsed
  in
  case matcher p step.request.url of
     Just path => do
       result <- handler $ { request.url := path } step
       pure $ { request.url := step.request.url } result
     Nothing => empty

