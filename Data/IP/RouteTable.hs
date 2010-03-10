{-|
  IP routing table is a tree of 'IPRange'
  to search one of them on the longest
  match base. It is a kind of TRIE with one
  way branching removed. Both IPv4 and IPv6
  are supported.
-}
module Data.IP.RouteTable (IPRTable, empty, insert, lookup, fromList, toList) where

import Prelude hiding (lookup)
import Data.IP.RouteTable.Internal
