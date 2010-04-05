module Data.IP.IP where

import Data.Bits
import Data.IP.Addr
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
class Eq a => IP a where
    {-|
      The 'masked' function takes an 'IP' address and a contiguous
      'IP' mask and returned a masked 'IP' address.
    -}
    masked :: a -> a -> a
    {-|
      The 'intToMask' function takes 'Int' and returns a contiguous
      'IP' mask.
    -}
    intToMask :: Int -> a
    {-|
      The 'intToTBit' function takes 'Int' and returns an 'IP' address
      whose only n-th bit is set.
    -}
    intToTBit   :: Int -> a
    {-|
      The 'isZero' function takes an 'IP' address and an test bit 'IP'
      address and returns 'True' is the bit is unset, otherwise
      returns 'False'.
    -}
    isZero :: a -> a -> Bool
    version :: a -> IPVersion

instance IP IPv4 where
    IPv4 a `masked` IPv4 m = IPv4 (a .&. m)
    intToMask = maskIPv4
    intToTBit = intToTBitIPv4
    isZero a b = a `masked` b == IPv4 0
    version _ = IPVersion4

instance IP IPv6 where
    IPv6 (a1,a2,a3,a4) `masked` IPv6 (m1,m2,m3,m4) =
        IPv6 (a1.&.m1,a2.&.m2,a3.&.m3,a4.&.m4)
    intToMask = maskIPv6
    intToTBit = intToTBitIPv6
    isZero a b = a `masked` b == IPv6 (0,0,0,0)
    version _ = IPVersion6

----------------------------------------------------------------
--
-- Version
--

data IPVersion = IPVersion4 | IPVersion6 deriving (Eq, Ord)

isIPv4 :: (IP a) => a -> Bool
isIPv4 x = version x == IPVersion4

isIPv6 :: (IP a) => a -> Bool
isIPv6 x = version x == IPVersion6

----------------------------------------------------------------
--
-- Test Bit
--

intToTBitIPv4 :: Int -> IPv4
intToTBitIPv4 len = IPv4 (intToTBitsIPv4 ! len)

intToTBitIPv6 :: Int -> IPv6
intToTBitIPv6 len = IPv6 (intToTBitsIPv6 ! len)

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
