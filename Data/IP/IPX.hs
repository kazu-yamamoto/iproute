module Data.IP.IPX where

import Data.IP.Addr
import Data.IP.Range
import Data.IP.Routable

----------------------------------------------------------------

class IPX a where
    {-|
      The 'toIPX' function take an 'IP' address and returns 'IPX'.
    -}
    toIP :: a -> IP
    {-|
      The 'toIPXRange' function take an 'IPRange' address and
      returns 'IPXRange'.
    -}
    toIPRange :: a -> Int -> IPRange

instance IPX IPv4 where
    toIP a = IPv4 a
    toIPRange a len = IPv4Range (makeAddrRange a len)

instance IPX IPv6 where
    toIP a = IPv6 a
    toIPRange a len = IPv6Range (makeAddrRange a len)
