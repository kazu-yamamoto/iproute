{-|
  Data structures to express IPv4, IPv6 and IP range.
-}
module Data.IP (
    IP (masked, intToMask, intToTBit, isZero)
  , IPv4, toIPv4
  , IPv6, toIPv6
  , IPRange (addr, mask, mlen)
  , makeIPRange, (>:>), isMatchedTo
  , IPX (..)
  , IPXRange (..)
  , IPUnified (toIPX, toIPXRange)
  ) where

import Data.IP.Addr
import Data.IP.IP
import Data.IP.IPX
import Data.IP.Range
