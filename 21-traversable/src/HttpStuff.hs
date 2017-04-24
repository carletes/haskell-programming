module HttpStuff where

import qualified Data.ByteString.Lazy as L
import Network.Wreq


type Url = String

traverseUrls :: [Url] -> IO [Response L.ByteString]
traverseUrls urls = traverse get urls
