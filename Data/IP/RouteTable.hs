{-|
  IP routing table is a tree of 'IPRange'
  to search one of them on the longest
  match base. It is a kind of TRIE with one
  way branching removed. Both IPv4 and IPv6
  are supported.

  For more information, see:
      <http://www.mew.org/~kazu/proj/iproute/>
-}
module Data.IP.RouteTable (
  -- * Documentation
  -- ** Routable class
    Routable (..)
  -- ** Type for IP routing table
  , IPRTable
  -- ** Functions to manipulate an IP routing table
  , empty, insert, delete
  , Data.IP.RouteTable.Internal.lookup
  , Data.IP.RouteTable.Internal.subLookup
  , fromList, toList
  ) where

import Data.IP.RouteTable.Internal
