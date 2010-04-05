{-# LANGUAGE FlexibleInstances #-}
module Data.IP.Range where

import Control.Monad
import Data.IP.Addr
import Data.IP.IP
import Data.IP.Mask
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------
--
-- Range
--

{-|
  The IP range consists of an 'IP' address, a contiguous 'IP' mask,
  and mask length. The contiguous 'IP' mask and the mask length
  are essentially same information but contained for pre
  calculation.

  To create this, use 'makeIPRange' or 'read' \"192.0.2.0/24\" :: 'IPRange' 'IP
-}
data IP a => IPRange a =
    IPRange {
        -- |The 'addr' function returns an 'IP' address from 'IPRange'.
        addr :: a
        -- |The 'mask' function returns a contiguous 'IP' mask from 'IPRange'.
      , mask :: a
        -- |The 'mlen' function returns a mask length from 'IPRange'.
      , mlen :: Int
    } deriving (Eq, Ord)

----------------------------------------------------------------
--
-- Exported functions
--

{-|
  The >:> operator takes two 'IPRange'. It returns 'True' if
  the first 'IPRange' contains the second 'IPRange'. Otherwise,
  it returns 'False'.
-}
(>:>) :: IP a => IPRange a -> IPRange a -> Bool
a >:> b = mlen a <= mlen b && (addr b `masked` mask a) == addr a

{-|
  The 'toMatchedTo' function take an 'IP' address and an 'IPRange',
  and returns 'True' if the range contains the address.
-}

isMatchedTo :: IP a => a -> IPRange a -> Bool
isMatchedTo a r = a `masked` mask r == addr r

{-|
  The 'makeIPRange' functions takes an 'IP' address and a mask
  length. It creates a bit mask from the mask length and masks
  the 'IP' address, then returns 'IPRange' made of them.
-}
makeIPRange :: IP a => a -> Int -> IPRange a
makeIPRange ad len = IPRange adr msk len
   where
     msk = intToMask len
     adr = ad `masked` msk

----------------------------------------------------------------
--
-- Show
--

instance (IP a, Show a) => Show (IPRange a) where
    show x = show (addr x) ++ "/" ++ show (mlen x)

----------------------------------------------------------------
--
-- Read
--

instance Read (IPRange IPv4) where
    readsPrec _ = parseIPv4Range

instance Read (IPRange IPv6) where
    readsPrec _ = parseIPv6Range

parseIPv4Range :: String -> [(IPRange IPv4,String)]
parseIPv4Range cs = case parse (adopt ipv4range) "parseIPv4" cs of
                      Right r4 -> r4
                      Left  _  -> error "parseIPv4"

parseIPv6Range :: String -> [(IPRange IPv6,String)]
parseIPv6Range cs = case parse (adopt ipv6range) "parseIPv6" cs of
                      Right r6 -> r6
                      Left  _  -> error "parseIPv6"


ipv4range :: Parser (IPRange IPv4)
ipv4range = do
    ip <- ipv4
    len <- option 32 $ do { char '/'; dig }
    check len
    return $ IPRange ip (maskIPv4 len) len
  where
    check len = when (len < 0 || 32 < len) (unexpected "IPv4 mask length")

ipv6range :: Parser (IPRange IPv6)
ipv6range = do
    ip <- ipv6
    len <- option 128 $ do { char '/'; dig }
    check len
    return $ IPRange ip (maskIPv6 len) len
  where
    check len = when (len < 0 || 128 < len) (unexpected ("IPv6 mask length: " ++ show len))
