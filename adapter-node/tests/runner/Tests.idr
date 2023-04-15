module Tests

import BaseDir

import Test.Golden.RunnerHelper

main : IO ()
main = do
 goldenRunner 
  [ "http" `atDir` "server/http"
  , "http2" `atDir` "server/http2"
  ]

