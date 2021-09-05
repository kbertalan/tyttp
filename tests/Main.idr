module Main

import Test.Golden

basic : TestPool
basic = MkTestPool "Basic examples" [] Nothing [ "basic" ]

main : IO ()
main = runner [ basic ]
