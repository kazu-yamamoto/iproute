module Data.IP.IPX where

import Data.IP.Addr
import Data.IP.IP
import Data.IP.Range

----------------------------------------------------------------

data IPX = IP4 { toIP4 :: IPv4 }
         | IP6 { toIP6 :: IPv6 }
         deriving (Eq,Show)

data IPXRange = IP4Range { toIP4Range :: IPRange IPv4 }
              | IP6Range { toIP6Range :: IPRange IPv6 }
              deriving (Eq,Show)

----------------------------------------------------------------

class IP a => IPUnified a where
    {-|
      The 'toIPX' function take an 'IP' address and returns 'IPX'.
    -}
    toIPX :: a -> IPX
    {-|
      The 'toIPXRange' function take an 'IPRange' address and
      returns 'IPXRange'.
    -}
    toIPXRange :: a -> Int -> IPXRange

instance IPUnified IPv4 where
    toIPX a = IP4 a
    toIPXRange a len = IP4Range (makeIPRange a len)

instance IPUnified IPv6 where
    toIPX a = IP6 a
    toIPXRange a len = IP6Range (makeIPRange a len)
