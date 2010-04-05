{-# LANGUAGE FlexibleInstances #-}
module Data.IP.Range where

import Control.Monad
import Data.IP.Addr
import Data.IP.Mask
import Text.Parsec
import Text.Parsec.String

----------------------------------------------------------------

{-|
  A unified data for 'AddrRange' 'IPv4' and 'AddrRange' 'IPv6'.
-}

data IPRange = IPv4Range { ipv4range :: AddrRange IPv4 }
             | IPv6Range { tpv6range :: AddrRange IPv6 }
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
parseIPv4Range cs = case parse (adopt ip4range) "parseIPv4" cs of
                      Right r4 -> r4
                      Left  _  -> error "parseIPv4"

parseIPv6Range :: String -> [(AddrRange IPv6,String)]
parseIPv6Range cs = case parse (adopt ip6range) "parseIPv6" cs of
                      Right r6 -> r6
                      Left  _  -> error "parseIPv6"


ip4range :: Parser (AddrRange IPv4)
ip4range = do
    ip <- ip4
    len <- option 32 $ do { char '/'; dig }
    check len
    return $ AddrRange ip (maskIPv4 len) len
  where
    check len = when (len < 0 || 32 < len) (unexpected "IPv4 mask length")

ip6range :: Parser (AddrRange IPv6)
ip6range = do
    ip <- ip6
    len <- option 128 $ do { char '/'; dig }
    check len
    return $ AddrRange ip (maskIPv6 len) len
  where
    check len = when (len < 0 || 128 < len) (unexpected ("IPv6 mask length: " ++ show len))
