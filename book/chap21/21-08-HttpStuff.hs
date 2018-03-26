-- 21-08-HttpStuff.hs
--
-- 21.8 Do all the things, page 850
--

module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

-- replace with other websites if desired or needed
urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

sequencedUrls :: IO [Response ByteString]
sequencedUrls = sequence mappingGet

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls
