{-# LANGUAGE FlexibleInstances #-}
module Data.IP.Range where

import Data.Bits
import Control.Monad
import Data.IP.Addr
import Data.IP.Mask
import Text.Appar.String

----------------------------------------------------------------

{-|
  A unified data for 'AddrRange' 'IPv4' and 'AddrRange' 'IPv6'.
-}

data IPRange = IPv4Range { ipv4range :: AddrRange IPv4 }
             | IPv6Range { ipv6range :: AddrRange IPv6 }
             deriving (Eq,Show)

----------------------------------------------------------------
--
-- Range
--

{-|
  The Addr range consists of an address, a contiguous mask,
  and mask length. The contiguous mask and the mask length
  are essentially same information but contained for pre
  calculation.

  To create this, use 'makeAddrRange' or 'read' \"192.0.2.0/24\" :: 'AddrRange' 'IPv4'
-}
data AddrRange a = AddrRange {
        -- |The 'addr' function returns an address from 'AddrRange'.
        addr :: a
        -- |The 'mask' function returns a contiguous 'IP' mask from 'AddrRange'.
      , mask :: a
        -- |The 'mlen' function returns a mask length from 'AddrRange'.
      , mlen :: Int
    } deriving (Eq, Ord)

----------------------------------------------------------------
--
-- Show
--

instance Show a => Show (AddrRange a) where
    show x = show (addr x) ++ "/" ++ show (mlen x)

----------------------------------------------------------------
--
-- Read
--

instance Read (AddrRange IPv4) where
    readsPrec _ = parseIPv4Range

instance Read (AddrRange IPv6) where
    readsPrec _ = parseIPv6Range

parseIPv4Range :: String -> [(AddrRange IPv4,String)]
parseIPv4Range cs = case runParser ip4range cs of
    (Nothing,_)    -> error "parseIPv4Range"
    (Just a4,rest) -> [(a4,rest)]

parseIPv6Range :: String -> [(AddrRange IPv6,String)]
parseIPv6Range cs = case runParser ip6range cs of
    (Nothing,_)    -> error "parseIPv6Range"
    (Just a6,rest) -> [(a6,rest)]

ip4range :: Parser (AddrRange IPv4)
ip4range = do
    ip <- ip4
    len <- option 32 $ do { char '/'; dig }
    check len
    let msk = maskIPv4 len
        adr = ip `maskedIPv4` msk
    return $ AddrRange adr msk len
  where
    check len = when (len < 0 || 32 < len) (error "IPv4 mask length")

maskedIPv4 :: IPv4 -> IPv4 -> IPv4
IP4 a `maskedIPv4` IP4 m = IP4 (a .&. m)

ip6range :: Parser (AddrRange IPv6)
ip6range = do
    ip <- ip6
    len <- option 128 $ do { char '/'; dig }
    check len
    let msk = maskIPv6 len
        adr = ip `maskedIPv6` msk
    return $ AddrRange adr msk len
  where
    check len = when (len < 0 || 128 < len) (error ("IPv6 mask length: " ++ show len))

maskedIPv6 :: IPv6 -> IPv6 -> IPv6
IP6 (a1,a2,a3,a4) `maskedIPv6` IP6 (m1,m2,m3,m4) = IP6 (a1.&.m1,a2.&.m2,a3.&.m3,a4.&.m4)
