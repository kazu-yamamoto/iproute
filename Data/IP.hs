{-|
  Data structures to express IPv4, IPv6 and IP range.
-}
module Data.IP (
    IP (..)
  , IPv4, toIPv4
  , IPv6, toIPv6
  , IPRange (..)
  , AddrRange (addr, mask, mlen)
  , IPX (..)
  , Routable (..)
  , makeAddrRange, (>:>), isMatchedTo
  ) where

import Data.IP.Addr
import Data.IP.Range
import Data.IP.Routable
import Data.IP.IPX
