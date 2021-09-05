module Main

import Handler
import Handler.Combinators

exampleBasic : Step String String -> IO ()
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
  let step = MkStep (MkRequest "request") (MkResponse OK "")
  exampleBasic step
