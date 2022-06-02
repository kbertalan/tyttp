module Main

import Test.Golden

url : TestPool
url = MkTestPool "URL tests" [] Nothing [ "url" ]

node : TestPool
node = MkTestPool "Node tests" [] (Just Node) [ "node/uri" ]

server : TestPool
server = MkTestPool "Server" [] (Just Node) [ "server/http/echo", "server/http/files", "server/http2/echo", "server/http2/push" ]

ext : TestPool
ext = MkTestPool "Extensions" [] Nothing [ "ext/buffer" ]

main : IO ()
main = runner [ url, node, server, ext ]
