module Main

import Test.Golden

url : TestPool
url = MkTestPool "URL tests" [] Nothing [ "url" ]

server : TestPool
server = MkTestPool "Server" [] (Just Node) [ "server/http/echo", "server/http/files", "server/http2/echo", "server/http2/push" ]

main : IO ()
main = runner [ url, server ]
