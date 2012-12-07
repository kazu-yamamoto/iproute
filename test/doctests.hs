module Main where

import Test.DocTest

main :: IO ()
main = doctest ["-XOverloadedStrings", "Data/IP.hs", "Data/IP/RouteTable.hs"]
