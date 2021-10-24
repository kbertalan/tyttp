module TyTTP.Path

public export
data PathPattern : Type where
  Literal : String -> PathPattern
  Param : String -> PathPattern
  Rest : PathPattern

export
Show PathPattern where
  show (Literal s) = "Literal \{s}"
  show (Param s) = "Param \{s}"
  show Rest = "Rest"

export
Eq PathPattern where
  (==) (Literal s1) (Literal s2) = s1 == s2
  (==) (Param s1) (Param s2) = s1 == s2
  (==) Rest Rest = True
  (==) _ _ = False

data ParseState
  = InLiteral (List Char)
  | InParam (List Char)
  | InRest

export
parsePattern : String -> Maybe $ List PathPattern
parsePattern = map reverse . parse (InLiteral []) [] . unpack
  where
    collect : List Char -> String
    collect = pack . reverse

    allowed : List Char
    allowed = map chr $ [ord '-', ord '_'] ++ [ x | x <- [ord 'a' .. ord 'z']] ++ [ x | x <- [ord 'A' .. ord 'Z']] 

    parse : ParseState -> List PathPattern -> List Char -> Maybe $ List PathPattern
    parse (InLiteral s) p ('{' :: xs) = parse (InParam []) (Literal (collect s) :: p) xs
    parse (InLiteral s) p ('*' :: xs) = parse InRest (Literal (collect s) :: p) xs
    parse (InLiteral s) p ['/'] = Just $ Literal (collect s) :: p
    parse (InLiteral []) p [] = Just p
    parse (InLiteral s) p [] = Just $ Literal (collect s) :: p
    parse (InLiteral s) p (x :: xs) = parse (InLiteral $ x :: s) p xs
    parse (InParam s) p ('}' :: xs) = 
      let param = Param $ collect s
      in if elem param p || null s
      then Nothing
      else parse (InLiteral []) (param :: p) xs
    parse (InParam s) p [] = Nothing
    parse (InParam s) p (x :: xs) =
      if elem x allowed
      then parse (InParam (x :: s)) p xs
      else Nothing
    parse InRest p [] = Just $ Rest :: p
    parse InRest p (x :: _) = Nothing


