-- 19-05-Socket.hs
--
-- 19.05 Monad, page 806
-- Opening a network socket, page 806
--
--
module Socket where

import Network.Socket
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS

openSocket :: FilePath -> IO Socket
openSocket p = do
  sock <- socket AF_UNIX Stream defaultProtocol
  connect sock sockAddr
  return sock
  where
    sockAddr = SockAddrUnix . encodeString $ p
