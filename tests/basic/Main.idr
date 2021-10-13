module Main

import TyTTP
import TyTTP.Combinators

exampleBasic : Step Method () TyTTP.Request.simpleBody () String String -> IO ()
exampleBasic initialStep = do
  let handlers =
        [ hId
        , hEcho
        , hMapRequest (<+> "-appended") >=> hEcho
        , hEcho >=> hMapResponse (<+> "-response")
        , hConstRequest "const-request" >=> hEcho
        , hEcho >=> hConstResponse "const-response"
        ]
  for_ handlers $ \handler => do
      result <- handler initialStep
      putStrLn result.response.body

main : IO ()
main = do
  let step = MkStep (MkRequest GET () "request") (MkResponse OK () "")
  exampleBasic step
