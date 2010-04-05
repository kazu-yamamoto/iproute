{-|
  IP routing table is a tree of 'IPRange'
  to search one of them on the longest
  match base. It is a kind of TRIE with one
  way branching removed. Both IPv4 and IPv6
  are supported.
-}
module Data.IP.RouteTable (
    Routable (..)
  , IPRTable, empty, insert
  , Data.IP.RouteTable.Internal.lookup
  , fromList, toList
  ) where

import Data.IP.RouteTable.Internal
