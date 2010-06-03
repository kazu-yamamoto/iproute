module Main where

import Control.Exception
import Data.IP
import Data.IP.RouteTable
import Prelude hiding (lookup, catch)
import System.Environment

main :: IO ()
main = flip catch handler $ do
    as <- getArgs
    let file = as !! 0
        ip = as !! 1
        range = ip ++ "/32"
        target = readIPv4Range range
    rt <- loadRoutes file
    let res = lookup target rt
    print res
  where
    handler :: ErrorCall -> IO ()
    handler _ = help

loadRoutes :: FilePath -> IO (IPRTable IPv4 (AddrRange IPv4))
loadRoutes file = do
    cs <- readFile file
    let ls = lines cs
        rs = map readIPv4Range ls
        kvs = zip rs rs -- value is AddrRange itself in this test
        radish = fromList kvs
    return radish

readIPv4Range :: String -> AddrRange IPv4
readIPv4Range = read

help :: IO ()
help = do
  putStrLn "Usage:"
  putStrLn "        ipv4_search file IPv4"

