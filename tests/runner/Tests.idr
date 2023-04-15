module Tests

import BaseDir

import Test.Golden.RunnerHelper

main : IO ()
main = do
 goldenRunner [ "basics" `atDir` "basics" ]

