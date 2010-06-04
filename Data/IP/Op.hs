module Data.IP.Op where

import Data.Bits
import Data.IP.Addr
import Data.IP.Mask
import Data.IP.Range

----------------------------------------------------------------

class Eq a => Addr a where
    {-|
      The 'masked' function takes an 'Addr' and a contiguous
      mask and returned a masked 'Addr'.
    -}
    masked :: a -> a -> a
    {-|
      The 'intToMask' function takes 'Int' and returns a contiguous
      mask.
    -}
    intToMask :: Int -> a

instance Addr IPv4 where
    masked    = maskedIPv4
    intToMask = maskIPv4

instance Addr IPv6 where
    IP6 (a1,a2,a3,a4) `masked` IP6 (m1,m2,m3,m4) =
        IP6 (a1.&.m1,a2.&.m2,a3.&.m3,a4.&.m4)
    intToMask = maskIPv6

----------------------------------------------------------------

{-|
  The >:> operator takes two 'AddrRange'. It returns 'True' if
  the first 'AddrRange' contains the second 'AddrRange'. Otherwise,
  it returns 'False'.
-}
(>:>) :: Addr a => AddrRange a -> AddrRange a -> Bool
a >:> b = mlen a <= mlen b && (addr b `masked` mask a) == addr a

{-|
  The 'toMatchedTo' function take an 'Addr' address and an 'AddrRange',
  and returns 'True' if the range contains the address.
-}

isMatchedTo :: Addr a => a -> AddrRange a -> Bool
isMatchedTo a r = a `masked` mask r == addr r

{-|
  The 'makeAddrRange' functions takes an 'Addr' address and a mask
  length. It creates a bit mask from the mask length and masks
  the 'Addr' address, then returns 'AddrRange' made of them.
-}
makeAddrRange :: Addr a => a -> Int -> AddrRange a
makeAddrRange ad len = AddrRange adr msk len
   where
     msk = intToMask len
     adr = ad `masked` msk
