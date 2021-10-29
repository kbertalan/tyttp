module TyTTP.Path

import Data.List
import Data.String
import public Data.Maybe
import TyTTP

data Pattern : Type where
  Literal : List Char -> Pattern
  Param : List Char -> Pattern
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

export
data ParsedPattern : (0 s : String) -> Type where
  MkParsedPattern : List Pattern -> ParsedPattern s

public export
parse : (s : String) -> Maybe (ParsedPattern s)
parse s = map (MkParsedPattern . reverse) $ go (InLiteral []) [] $ unpack s
  where
    isAllowedInParam : Char -> Bool
    isAllowedInParam c = isAlpha c || c == '-' || c == '_'

    go : ParseState -> List Pattern -> List Char -> Maybe $ List Pattern
    go (InLiteral []) p ('{' :: xs) = Nothing
    go (InLiteral s) p ('{' :: xs) = go (InParam []) (Literal (reverse s) :: p) xs
    go (InLiteral []) p ('*' :: xs) = Nothing
    go (InLiteral s) p ('*' :: xs) = go InRest (Literal (reverse s) :: p) xs
    go (InLiteral s) p ['/'] = Just $ Literal (reverse s) :: p
    go (InLiteral []) p [] = Just p
    go (InLiteral s) p [] = Just $ Literal (reverse s) :: p
    go (InLiteral s) p (x :: xs) = go (InLiteral $ x :: s) p xs
    go (InParam s) p ('}' :: xs) = 
      let param = Param $ reverse s
      in if elem param p || null s
      then Nothing
      else go (InLiteral []) (param :: p) xs
    go (InParam s) p [] = Nothing
    go (InParam []) p (x :: xs) =
      if isAlpha x
      then go (InParam [x]) p xs
      else Nothing
    go (InParam s) p (x :: xs) =
      if isAllowedInParam x
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

matcher : (s : String) -> ParsedPattern str -> Maybe Path
matcher s (MkParsedPattern ls) = go ls (unpack s) $ MkPath s [] ""
  where

    consumeLiteral : List Char -> List Char -> Maybe $ List Char
    consumeLiteral [] xs = Just xs
    consumeLiteral (_::_) [] = Nothing
    consumeLiteral (l::ls) (x::xs) =
      case l == x of
        True => consumeLiteral ls xs
        False => Nothing

    go : List Pattern -> List Char -> Path -> Maybe Path
    go [] [] p = Just p
    go [] xs _ = Nothing
    go (Literal l :: ps) xs p = do
      remaining <- consumeLiteral l xs
      go ps remaining p
    go (Param param :: Literal l@(f::fs) :: ps) xs p =
        let (value, remaining) = List.break (==f) xs
        in if null value
        then Nothing
        else go (Literal l :: ps) remaining $ { params $= ((pack param, pack value)::) } p
    go (Param param :: Nil) xs p =
      if null xs
      then Nothing
      else Just $ { params $= ((pack param, pack xs)::) } p
    go (Rest :: Nil) xs p = Just $ { rest := pack xs } p
    go _ _ _ = Nothing


export
pattern : Monad m 
  => Alternative m 
  => (str : String)
  -> {default (parse str) parsed : Maybe $ ParsedPattern str}
  -> {auto 0 ok : IsJust parsed }
  -> (
    Step me Path h1 fn st h2 a b
    -> m $ Step me' Path h1' fn' st' h2' a' b'
  )
  -> Step me String h1 fn st h2 a b
  -> m $ Step me' String h1' fn' st' h2' a' b'
pattern str handler step =
  let Just p = parsed
  in
  case matcher step.request.url p of
     Just path => do
       result <- handler $ { request.url := path } step
       pure $ { request.url := step.request.url } result
     Nothing => empty

