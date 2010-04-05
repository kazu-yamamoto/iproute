module Data.IP.Routable where

import Data.Bits
import Data.IP.Addr
import Data.IP.Range
import Data.IP.Mask
import Data.IntMap hiding (map)
import Data.Word

----------------------------------------------------------------
--
-- IP Class
--

{-|
  A class to contain IPv4 and IPv6.
-}
class Eq a => Routable a where
    {-|
      The 'masked' function takes an 'Routable' address and a contiguous
      'Routable' mask and returned a masked 'Routable' address.
    -}
    masked :: a -> a -> a
    {-|
      The 'intToMask' function takes 'Int' and returns a contiguous
      'Routable' mask.
    -}
    intToMask :: Int -> a
    {-|
      The 'intToTBit' function takes 'Int' and returns an 'Routable' address
      whose only n-th bit is set.
    -}
    intToTBit   :: Int -> a
    {-|
      The 'isZero' function takes an 'Routable' address and an test bit
      'Routable' address and returns 'True' is the bit is unset,
      otherwise returns 'False'.
    -}
    isZero :: a -> a -> Bool

instance Routable IPv4 where
    IP4 a `masked` IP4 m = IP4 (a .&. m)
    intToMask = maskIPv4
    intToTBit = intToTBitIPv4
    isZero a b = a `masked` b == IP4 0

instance Routable IPv6 where
    IP6 (a1,a2,a3,a4) `masked` IP6 (m1,m2,m3,m4) =
        IP6 (a1.&.m1,a2.&.m2,a3.&.m3,a4.&.m4)
    intToMask = maskIPv6
    intToTBit = intToTBitIPv6
    isZero a b = a `masked` b == IP6 (0,0,0,0)

----------------------------------------------------------------
--
-- Test Bit
--

intToTBitIPv4 :: Int -> IPv4
intToTBitIPv4 len = IP4 (intToTBitsIPv4 ! len)

intToTBitIPv6 :: Int -> IPv6
intToTBitIPv6 len = IP6 (intToTBitsIPv6 ! len)

intToTBitsWord32 :: [Word32]
intToTBitsWord32 = iterate (flip shift (-1)) 0x80000000

intToTBitsIPv4 :: IntMap IPv4Addr
intToTBitsIPv4 = fromList $ zip [0..32] intToTBitsWord32

intToTBitsIPv6 :: IntMap IPv6Addr
intToTBitsIPv6 = fromList $ zip [0..128] bs
  where
    bs = b1 ++ b2 ++ b3 ++ b4 ++ b5
    b1 = map (\vbit -> (vbit,all0,all0,all0)) intToTBits
    b2 = map (\vbit -> (all0,vbit,all0,all0)) intToTBits
    b3 = map (\vbit -> (all0,all0,vbit,all0)) intToTBits
    b4 = map (\vbit -> (all0,all0,all0,vbit)) intToTBits
    b5 =              [(all0,all0,all0,all0)]
    intToTBits = take 32 $ intToTBitsWord32
    all0 = 0x00000000

----------------------------------------------------------------
--
-- Exported functions
--

{-|
  The >:> operator takes two 'AddrRange'. It returns 'True' if
  the first 'AddrRange' contains the second 'AddrRange'. Otherwise,
  it returns 'False'.
-}
(>:>) :: Routable a => AddrRange a -> AddrRange a -> Bool
a >:> b = mlen a <= mlen b && (addr b `masked` mask a) == addr a

{-|
  The 'toMatchedTo' function take an 'Routable' address and an 'AddrRange',
  and returns 'True' if the range contains the address.
-}

isMatchedTo :: Routable a => a -> AddrRange a -> Bool
isMatchedTo a r = a `masked` mask r == addr r

{-|
  The 'makeAddrRange' functions takes an 'Routable' address and a mask
  length. It creates a bit mask from the mask length and masks
  the 'Routable' address, then returns 'AddrRange' made of them.
-}
makeAddrRange :: Routable a => a -> Int -> AddrRange a
makeAddrRange ad len = AddrRange adr msk len
   where
     msk = intToMask len
     adr = ad `masked` msk

