-- |
--   Data structures to express IPv4, IPv6 and IP range.
module Data.IP (
    -- * IP data
    IP (..),

    -- ** IPv4
    IPv4,
    toIPv4,
    toIPv4w,
    fromIPv4,
    fromIPv4w,
    fromHostAddress,
    toHostAddress,

    -- ** IPv6
    IPv6,
    toIPv6,
    toIPv6b,
    toIPv6w,
    fromIPv6,
    fromIPv6b,
    fromIPv6w,
    fromHostAddress6,
    toHostAddress6,

    -- ** Converters
    ipv4ToIPv6,
    fromSockAddr,
    toSockAddr,

    -- * IP range data
    IPRange (..),
    AddrRange (addr, mask, mlen),

    -- * Address class
    Addr (..),
    makeAddrRange,
    (>:>),
    isMatchedTo,
    addrRangePair,
    ipv4RangeToIPv6,
) where

import Data.IP.Addr
import Data.IP.Op
import Data.IP.Range
