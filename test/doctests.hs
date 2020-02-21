module Main where

import Test.DocTest

main :: IO ()
main = doctest [ "-XOverloadedStrings"
               , "-package=appar"
               , "-package=byteorder"
               , "-package=network"
               , "Data/IP.hs"
               , "Data/IP/RouteTable.hs"]
