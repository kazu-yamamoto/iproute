{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.IP.Addr where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Data (Data)
import qualified Data.List as L
import Data.String
import Data.Word
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Enum (fromEnumError, predError, succError, toEnumError)
import GHC.Generics
import Network.Socket
import Numeric (showHex, showInt)
import Text.Appar.String

----------------------------------------------------------------

-- |
--   A unified IP data for 'IPv4' and 'IPv6'.
--   To create this, use the data constructors. Or use 'read' @\"192.0.2.1\"@ :: 'IP', for example. Also, @\"192.0.2.1\"@ can be used as literal with OverloadedStrings.
--
-- >>> (read "192.0.2.1" :: IP) == IPv4 (read "192.0.2.1" :: IPv4)
-- True
-- >>> (read "2001:db8:00:00:00:00:00:01" :: IP) == IPv6 (read "2001:db8:00:00:00:00:00:01" :: IPv6)
-- True
data IP
    = IPv4 {ipv4 :: IPv4}
    | IPv6 {ipv6 :: IPv6}
    deriving (Data, Generic)

-- |
--   Equality over IP addresses. Correctly compare IPv4 and IPv4-embedded-in-IPv6 addresses.
--
-- >>> (read "2001:db8:00:00:00:00:00:01" :: IP) == (read "2001:db8:00:00:00:00:00:01" :: IP)
-- True
-- >>> (read "2001:db8:00:00:00:00:00:01" :: IP) == (read "2001:db8:00:00:00:00:00:05" :: IP)
-- False
-- >>> (read "127.0.0.1" :: IP) == (read "127.0.0.1" :: IP)
-- True
-- >>> (read "127.0.0.1" :: IP) == (read "10.0.0.1" :: IP)
-- False
-- >>> (read "::ffff:127.0.0.1" :: IP) == (read "127.0.0.1" :: IP)
-- True
-- >>> (read "::ffff:127.0.0.1" :: IP) == (read "127.0.0.9" :: IP)
-- False
-- >>> (read "::ffff:127.0.0.1" :: IP) >= (read "127.0.0.1" :: IP)
-- True
-- >>> (read "::ffff:127.0.0.1" :: IP) <= (read "127.0.0.1" :: IP)
-- True
instance Eq IP where
    (IPv4 ip1) == (IPv4 ip2) = ip1 == ip2
    (IPv6 ip1) == (IPv6 ip2) = ip1 == ip2
    (IPv4 ip1) == (IPv6 ip2) = ipv4ToIPv6 ip1 == ip2
    (IPv6 ip1) == (IPv4 ip2) = ip1 == ipv4ToIPv6 ip2

instance Ord IP where
    (IPv4 ip1) `compare` (IPv4 ip2) = ip1 `compare` ip2
    (IPv6 ip1) `compare` (IPv6 ip2) = ip1 `compare` ip2
    (IPv4 ip1) `compare` (IPv6 ip2) = ipv4ToIPv6 ip1 `compare` ip2
    (IPv6 ip1) `compare` (IPv4 ip2) = ip1 `compare` ipv4ToIPv6 ip2

instance Show IP where
    show (IPv4 ip) = show ip
    show (IPv6 ip) = show ip

----------------------------------------------------------------

-- This is host byte order
type IPv4Addr = Word32
type IPv6Addr = (Word32, Word32, Word32, Word32)

-- |
--   The abstract data type to express an IPv4 address.
--   To create this, use 'toIPv4'. Or use 'read' @\"192.0.2.1\"@ :: 'IPv4', for example. Also, @\"192.0.2.1\"@ can be used as literal with OverloadedStrings.
--
-- >>> read "192.0.2.1" :: IPv4
-- 192.0.2.1
newtype IPv4 = IP4 IPv4Addr
    deriving (Eq, Ord, Bounded, Data, Generic)

-- |
--   The abstract data type to express an IPv6 address.
--   To create this, use 'toIPv6'. Or use 'read' @\"2001:DB8::1\"@ :: 'IPv6', for example. Also, @\"2001:DB8::1\"@ can be used as literal with OverloadedStrings.
--
-- >>> read "2001:db8:00:00:00:00:00:01" :: IPv6
-- 2001:db8::1
-- >>> read "2001:db8:11e:c00::101" :: IPv6
-- 2001:db8:11e:c00::101
-- >>> read "2001:db8:11e:c00:aa:bb:192.0.2.1" :: IPv6
-- 2001:db8:11e:c00:aa:bb:c000:201
-- >>> read "2001:db8::192.0.2.1" :: IPv6
-- 2001:db8::c000:201
-- >>> read "0::ffff:192.0.2.1" :: IPv6
-- ::ffff:192.0.2.1
-- >>> read "0::0:c000:201" :: IPv6
-- ::192.0.2.1
-- >>> read "::0.0.0.1" :: IPv6
-- ::1
newtype IPv6 = IP6 IPv6Addr
    deriving (Eq, Ord, Bounded, Data, Generic)

----------------------------------------------------------------
--
-- Enum
--

-- | Delegates to 'Word32', which stops at 'minBound' and 'maxBound'.
--
-- >>> [read "255.255.255.254" ..] :: [IPv4]
-- [255.255.255.254,255.255.255.255]
-- >>> [read "0.0.0.1", read "0.0.0.0" ..] :: [IPv4]
-- [0.0.0.1,0.0.0.0]
-- >>> [read "192.0.2.2" .. read "192.0.2.1"] :: [IPv4]
-- []
instance Enum IPv4 where
    fromEnum (IP4 a) = fromEnum a
    toEnum = IP4 . toEnum
    succ (IP4 a) = IP4 (succ a)
    pred (IP4 a) = IP4 (pred a)
    enumFrom (IP4 a) = IP4 <$> enumFrom a
    enumFromTo (IP4 a) (IP4 b) = IP4 <$> enumFromTo a b
    enumFromThen (IP4 a) (IP4 b) = IP4 <$> enumFromThen a b
    enumFromThenTo (IP4 a) (IP4 b) (IP4 c) = IP4 <$> enumFromThenTo a b c

-- | Stops at 'minBound' and 'maxBound'.
--   'fromEnum' and 'toEnum' fail if the address does not fit in 'Int'.
--
-- >>> [read "ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe" ..] :: [IPv6]
-- [ffff:ffff:ffff:ffff:ffff:ffff:ffff:fffe,ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff]
-- >>> [read "::2", read "::1" ..] :: [IPv6]
-- [::2,::1,::]
-- >>> [read "::2" .. read "::1"] :: [IPv6]
-- []
-- >>> fromEnum (read "::1:0" :: IPv6)
-- 65536
instance Enum IPv6 where
    fromEnum ip
        | i > toInteger (maxBound :: Int) = fromEnumError "IPv6" ip
        | otherwise = fromInteger i
      where
        i = ip6ToInteger ip
    toEnum i
        | i < 0 = toEnumError "IPv6" i (minBound :: IPv6, maxBound :: IPv6)
        | otherwise = integerToIP6 (toInteger i)

    succ (IP6 (0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff)) = succError "IPv6"
    succ (IP6 (a, 0xffffffff, 0xffffffff, 0xffffffff)) = IP6 (succ a, 0, 0, 0)
    succ (IP6 (a, b, 0xffffffff, 0xffffffff)) = IP6 (a, succ b, 0, 0)
    succ (IP6 (a, b, c, 0xffffffff)) = IP6 (a, b, succ c, 0)
    succ (IP6 (a, b, c, d)) = IP6 (a, b, c, succ d)

    pred (IP6 (0, 0, 0, 0)) = predError "IPv6"
    pred (IP6 (a, 0, 0, 0)) = IP6 (pred a, 0xffffffff, 0xffffffff, 0xffffffff)
    pred (IP6 (a, b, 0, 0)) = IP6 (a, pred b, 0xffffffff, 0xffffffff)
    pred (IP6 (a, b, c, 0)) = IP6 (a, b, pred c, 0xffffffff)
    pred (IP6 (a, b, c, d)) = IP6 (a, b, c, pred d)

    enumFrom ip = enumFromTo ip maxBound

    enumFromTo ip ip'
        | ip > ip' = []
        | otherwise = ip : gen ip
      where
        gen i
            | i == ip' = []
            | otherwise = let i' = succ i in i' : gen i'

    enumFromThen ip ip'
        | ip' >= ip = enumFromThenTo ip ip' maxBound
        | otherwise = enumFromThenTo ip ip' minBound

    -- Integer's enumFromThenTo never goes beyond the bounds given.
    enumFromThenTo ip inc fin =
        integerToIP6 <$> [ip6ToInteger ip, ip6ToInteger inc .. ip6ToInteger fin]

-- | Mixed families are enumerated as IPv6, as in 'Eq' and 'Ord'.
--
-- >>> [IPv4 (read "192.0.2.1") .. IPv6 (read "::ffff:192.0.2.2")]
-- [::ffff:192.0.2.1,::ffff:192.0.2.2]
instance Enum IP where
    fromEnum (IPv4 ip) = fromEnum ip
    fromEnum (IPv6 ip) = fromEnum ip

    -- Because Int cannot hold an IPv6 anyway
    toEnum = IPv4 . toEnum

    succ (IPv4 ip) = IPv4 $ succ ip
    succ (IPv6 ip) = IPv6 $ succ ip

    pred (IPv4 ip) = IPv4 $ pred ip
    pred (IPv6 ip) = IPv6 $ pred ip

    enumFrom (IPv4 ip) = IPv4 <$> enumFrom ip
    enumFrom (IPv6 ip) = IPv6 <$> enumFrom ip

    enumFromTo (IPv4 ip) (IPv4 ip') = IPv4 <$> enumFromTo ip ip'
    enumFromTo ip ip' = IPv6 <$> enumFromTo (toV6 ip) (toV6 ip')

    enumFromThen (IPv4 ip) (IPv4 ip') = IPv4 <$> enumFromThen ip ip'
    enumFromThen ip ip' = IPv6 <$> enumFromThen (toV6 ip) (toV6 ip')

    enumFromThenTo (IPv4 ip) (IPv4 inc) (IPv4 fin) = IPv4 <$> enumFromThenTo ip inc fin
    enumFromThenTo ip inc fin = IPv6 <$> enumFromThenTo (toV6 ip) (toV6 inc) (toV6 fin)

toV6 :: IP -> IPv6
toV6 (IPv4 ip) = ipv4ToIPv6 ip
toV6 (IPv6 ip) = ip

ip6ToInteger :: IPv6 -> Integer
ip6ToInteger (IP6 (a, b, c, d)) =
    let a' = word32ToInteger a `shift` 96
        b' = word32ToInteger b `shift` 64
        c' = word32ToInteger c `shift` 32
        d' = word32ToInteger d
     in a' .|. b' .|. c' .|. d'
  where
    word32ToInteger :: Word32 -> Integer
    word32ToInteger = toEnum . fromEnum

integerToIP6 :: Integer -> IPv6
integerToIP6 i =
    let a = integerToWord32 (i `shiftR` 96 .&. 0xffffffff)
        b = integerToWord32 (i `shiftR` 64 .&. 0xffffffff)
        c = integerToWord32 (i `shiftR` 32 .&. 0xffffffff)
        d = integerToWord32 (i .&. 0xffffffff)
     in IP6 (a, b, c, d)
  where
    integerToWord32 :: Integer -> Word32
    integerToWord32 = toEnum . fromEnum

----------------------------------------------------------------
--
-- Show
--

instance Show IPv4 where
    show ip = showIPv4 ip ""

instance Show IPv6 where
    show ip = showIPv6 ip ""

-- | Show an IPv4 address in the dot-decimal notation.
showIPv4 :: IPv4 -> ShowS
showIPv4 = foldr1 (.) . L.intersperse (showChar '.') . map showInt . fromIPv4

-- | Show an IPv6 address in the most appropriate notation, based on recommended
-- representation proposed by <http://tools.ietf.org/html/rfc5952 RFC 5952>.
--
-- /The implementation is completely compatible with the current implementation
-- of the `inet_ntop` function in glibc./
showIPv6 :: IPv6 -> ShowS
showIPv6 ip@(IP6 (a1, a2, a3, a4))
    -- IPv4-Mapped IPv6 Address
    | a1 == 0 && a2 == 0 && a3 == 0xffff =
        showString "::ffff:" . showIPv4 (IP4 a4)
    -- IPv4-Compatible IPv6 Address (exclude IPRange ::/112)
    | a1 == 0 && a2 == 0 && a3 == 0 && a4 >= 0x10000 =
        showString "::" . showIPv4 (IP4 a4)
    -- length of longest run > 1, replace it with "::"
    | end - begin > 1 =
        showFields prefix . showString "::" . showFields suffix
    -- length of longest run <= 1, don't use "::"
    | otherwise =
        showFields fields
  where
    fields = fromIPv6 ip
    showFields = foldr (.) id . L.intersperse (showChar ':') . map showHex
    prefix = take begin fields -- fields before "::"
    suffix = drop end fields -- fields after "::"
    begin = end + diff -- the longest run of zeros
    (diff, end) =
        minimum $
            scanl (\c i -> if i == 0 then c - 1 else 0) 0 fields `zip` [0 ..]

----------------------------------------------------------------
--
-- IntToIP
--

-- |
--   The 'toIPv4' function returns the 'IPv4' address corresponding to the given
--   list of 'Int' octets.  The function is strict in the four elements of the
--   list.  An error is returned if the list has a differnet length.  The input
--   elements are silently truncated to their 8 least-significant bits before they
--   are combined to form the IPv4 address.
--
-- >>> toIPv4 [192,0,2,1]
-- 192.0.2.1
toIPv4 :: [Int] -> IPv4
toIPv4 [a1, a2, a3, a4] = IP4 w
  where
    w =
        (fromIntegral a1 .&. 0xff) `unsafeShiftL` 24
            .|. (fromIntegral a2 .&. 0xff) `unsafeShiftL` 16
            .|. (fromIntegral a3 .&. 0xff) `unsafeShiftL` 8
            .|. (fromIntegral a4 .&. 0xff)
toIPv4 _ = error "IPv4 field list length != 4"
{-# INLINE toIPv4 #-}

-- |
--   The 'toIPv4w' function constructs the 'IPv4' address corresponding to the
--   given 'Word32' value.  Unlike the 'fromHostAddress' function, it is strict in
--   the input value, which here is in host byte order.
--
-- >>> toIPv4w 0xc0000201
-- 192.0.2.1
--
-- @since 1.7.9
toIPv4w :: Word32 -> IPv4
toIPv4w w = IP4 w
{-# INLINE toIPv4w #-}

-- |
--   The 'toIPv6' function returns the 'IPv6' address corresponding to the given
--   list of eight 16-bit 'Int's.  The function is strict in the eight elements of
--   the list.  An error is returned if the list has a differnet length.  The
--   input elements are in host byte order and are silently truncated to their 16
--   least-signicant bits before they are combined to form the IPv6 address.
--
-- >>> toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]
-- 2001:db8::1
toIPv6 :: [Int] -> IPv6
toIPv6 [i1, i2, i3, i4, i5, i6, i7, i8] = IP6 (x1, x2, x3, x4)
  where
    !x1 = fromIntegral $ (i1 .&. 0xffff) `unsafeShiftL` 16 .|. (i2 .&. 0xffff)
    !x2 = fromIntegral $ (i3 .&. 0xffff) `unsafeShiftL` 16 .|. (i4 .&. 0xffff)
    !x3 = fromIntegral $ (i5 .&. 0xffff) `unsafeShiftL` 16 .|. (i6 .&. 0xffff)
    !x4 = fromIntegral $ (i7 .&. 0xffff) `unsafeShiftL` 16 .|. (i8 .&. 0xffff)
toIPv6 _ = error "toIPv6 field list length != 8"
{-# INLINE toIPv6 #-}

-- |
--   The 'toIPv6b' function returns the IPv6 address corresponding to the given
--   list of sixteen 'Int' octets.  The function is strict in the sixteen elements
--   of the list.  An error is returned if the list has a differnet length.  The
--   input elements are silently truncated to their 8 least-signicant bits before
--   they are combined to form the IPv6 address.
--
-- >>> toIPv6b [0x20,0x01,0xD,0xB8,0,0,0,0,0,0,0,0,0,0,0,1]
-- 2001:db8::1
toIPv6b :: [Int] -> IPv6
toIPv6b
    [ h11
        , h12
        , l11
        , l12
        , h21
        , h22
        , l21
        , l22
        , h31
        , h32
        , l31
        , l32
        , h41
        , h42
        , l41
        , l42
        ] = IP6 (x1, x2, x3, x4)
      where
        !x1 =
            fromIntegral $
                (h11 .&. 0xff) `unsafeShiftL` 24
                    .|. (h12 .&. 0xff) `unsafeShiftL` 16
                    .|. (l11 .&. 0xff) `unsafeShiftL` 8
                    .|. (l12 .&. 0xff)
        !x2 =
            fromIntegral $
                (h21 .&. 0xff) `unsafeShiftL` 24
                    .|. (h22 .&. 0xff) `unsafeShiftL` 16
                    .|. (l21 .&. 0xff) `unsafeShiftL` 8
                    .|. (l22 .&. 0xff)
        !x3 =
            fromIntegral $
                (h31 .&. 0xff) `unsafeShiftL` 24
                    .|. (h32 .&. 0xff) `unsafeShiftL` 16
                    .|. (l31 .&. 0xff) `unsafeShiftL` 8
                    .|. (l32 .&. 0xff)
        !x4 =
            fromIntegral $
                (h41 .&. 0xff) `unsafeShiftL` 24
                    .|. (h42 .&. 0xff) `unsafeShiftL` 16
                    .|. (l41 .&. 0xff) `unsafeShiftL` 8
                    .|. (l42 .&. 0xff)
toIPv6b _ = error "toIPv6b field list length != 16"

-- |
--   The 'toIPv6w' function constructs the 'IPv6' address corresponding to the
--   given four-tuple of host byte order 'Word32' values.  This function differs
--   from the 'fromHostAddress6' function only in the fact that it is strict in
--   the elements of the tuple.
--
-- >>> toIPv6w (0x20010DB8,0x0,0x0,0x1)
-- 2001:db8::1
--
-- @since 1.7.9
toIPv6w :: (Word32, Word32, Word32, Word32) -> IPv6
toIPv6w w@(!_, !_, !_, !_) = IP6 w
{-# INLINE toIPv6w #-}

----------------------------------------------------------------
--
-- IPToInt
--

-- |
--   The 'fromIPv4' function returns the list of four 'Int' octets corresponding
--   to the given 'IPv4' address.
--
-- >>> fromIPv4 (toIPv4 [192,0,2,1])
-- [192,0,2,1]
fromIPv4 :: IPv4 -> [Int]
fromIPv4 (IP4 w) = [split w 0o30, split w 0o20, split w 0o10, split w 0]
  where
    split :: Word32 -> Int -> Int
    split a n = fromIntegral $ a `unsafeShiftR` n .&. 0xff
{-# INLINE fromIPv4 #-}

-- |
--   The 'fromIPv4w' function returns a single 'Word32' value corresponding to the
--   given the 'IPv4' address.  Unlike the 'toHostAddress' function, the returned
--   value is strictly evaluated, and is not converted to network byte order.
--
-- >>> fromIPv4w (toIPv4 [0xc0,0,2,1]) == 0xc0000201
-- True
--
-- @since 1.7.9
fromIPv4w :: IPv4 -> Word32
fromIPv4w (IP4 !ip4rep) = ip4rep
{-# INLINE fromIPv4w #-}

-- |
--   The 'fromIPv6' function returns a list eight 'Int's in host byte order
--   corresponding to the eight 16-bit fragments of the given IPv6 address.
--
-- >>> fromIPv6 (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1])
-- [8193,3512,0,0,0,0,0,1]
fromIPv6 :: IPv6 -> [Int]
fromIPv6 (IP6 (w1, w2, w3, w4)) =
    split w1 . split w2 . split w3 . split w4 $ []
  where
    split :: Word32 -> [Int] -> [Int]
    split n acc =
        fromIntegral (n `unsafeShiftR` 0x10 .&. 0xffff)
            : fromIntegral (n .&. 0xffff)
            : acc
{-# INLINE fromIPv6 #-}

-- |
--   The 'fromIPv6b' function returns the 16 'Int' octets corresponding
--   to the 16 bytes of the given IPv6 address.
--
-- >>> fromIPv6b (toIPv6b [0x20,0x01,0xD,0xB8,0,0,0,0,0,0,0,0,0,0,0,1])
-- [32,1,13,184,0,0,0,0,0,0,0,0,0,0,0,1]
fromIPv6b :: IPv6 -> [Int]
fromIPv6b (IP6 (w1, w2, w3, w4)) =
    split w1 . split w2 . split w3 . split w4 $ []
  where
    split :: Word32 -> [Int] -> [Int]
    split n acc =
        fromIntegral (n `unsafeShiftR` 24 .&. 0xff)
            : fromIntegral (n `unsafeShiftR` 16 .&. 0xff)
            : fromIntegral (n `unsafeShiftR` 8 .&. 0xff)
            : fromIntegral (n .&. 0xff)
            : acc

-- |
--   The 'fromIPv6w' function returns a four-tuple of 'Word32' values in host byte
--   order corresponding to the given 'IPv6' address.  This is identical to the
--   'toHostAddress6' function, except that the elements of four-tuple are
--   first strictly evaluated.
--
-- >>> fromIPv6w (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) == (0x20010DB8, 0, 0, 1)
-- True
--
-- @since 1.7.9
fromIPv6w :: IPv6 -> (Word32, Word32, Word32, Word32)
fromIPv6w (IP6 ip6rep) = ip6rep
{-# INLINE fromIPv6w #-}

----------------------------------------------------------------
--
-- Read
--

-- |
-- >>> read "[192.0.2.1,2001:db8::1]" :: [IP]
-- [192.0.2.1,2001:db8::1]
instance Read IP where
    readsPrec _ = parseIP

-- | 'reads' returns the rest of the input.
--
-- >>> reads "192.0.2.1 rest" :: [(IPv4, String)]
-- [(192.0.2.1," rest")]
-- >>> read "(192.0.2.1,80)" :: (IPv4, Int)
-- (192.0.2.1,80)
-- >>> reads "192.0.2.1a" :: [(IPv4, String)]
-- []
instance Read IPv4 where
    readsPrec _ = parseIPv4

-- | 'reads' returns the rest of the input.
--
-- >>> reads "::192.0.2.1 rest" :: [(IPv6, String)]
-- [(::192.0.2.1," rest")]
-- >>> read "[::1,2001:db8::1]" :: [IPv6]
-- [::1,2001:db8::1]
instance Read IPv6 where
    readsPrec _ = parseIPv6

parseIP :: String -> [(IP, String)]
parseIP cs = case runParser ip4 cs of
    (Just ip, rest) -> [(IPv4 ip, rest)]
    (Nothing, _) -> case runParser ip6 cs of
        (Just ip, rest) -> [(IPv6 ip, rest)]
        (Nothing, _) -> []

parseIPv4 :: String -> [(IPv4, String)]
parseIPv4 cs = case runParser ip4 cs of
    (Nothing, _) -> []
    (Just a4, rest) -> [(a4, rest)]

parseIPv6 :: String -> [(IPv6, String)]
parseIPv6 cs = case runParser ip6 cs of
    (Nothing, _) -> []
    (Just a6, rest) -> [(a6, rest)]

----------------------------------------------------------------
--
-- IsString
--

instance IsString IP where
    fromString = read

instance IsString IPv4 where
    fromString = read

instance IsString IPv6 where
    fromString = read

----------------------------------------------------------------
--
-- IPv4 Parser
--

octet :: Parser Int
octet =
    0 <$ char '0'
        <|> (toInt =<< (:) <$> oneOf ['1' .. '9'] <*> many digit)
  where
    toInt ds = maybe (fail "IPv4 address") pure $ foldr go Just ds 0
    go !d !f !n =
        let n' = n * 10 + ord d - 48
         in if n' <= 255 then f n' else Nothing

ip4 :: Parser IPv4
ip4 = skipSpaces >> toIPv4 <$> ip4'

ip4' :: Parser [Int]
ip4' = do
    a0 <- octet
    _ <- char '.'
    a1 <- octet
    _ <- char '.'
    a2 <- octet
    _ <- char '.'
    a3 <- octet
    boundary
    return [a0, a1, a2, a3]

skipSpaces :: Parser ()
skipSpaces = void $ many (char ' ')

-- | Succeeds without consuming input if the address cannot continue here,
--   so that 'readsPrec' can return the rest of the input.
boundary :: Parser ()
boundary = P $ \str -> case str of
    c : _ | isHexDigit c || c == '.' || c == ':' -> (Nothing, str)
    _ -> (Just (), str)

----------------------------------------------------------------
--
-- IPv6 Parser (RFC 4291)
--

hex :: Parser Int
hex = do
    ns <- some hexDigit
    check ns
    let ms = map digitToInt ns
        val = L.foldl' (\x y -> x * 16 + y) 0 ms
    return val
  where
    check ns = when (length ns > 4) (fail "IPv6 address -- more than 4 hex")

colon2 :: Parser ()
colon2 = void $ string "::"

format :: [Int] -> [Int] -> Parser [Int]
format bs1 bs2 = do
    let len1 = length bs1
        len2 = length bs2
    when (len1 > 7) (fail "IPv6 address1")
    when (len2 > 7) (fail "IPv6 address2")
    let len = 8 - len1 - len2
    when (len <= 0) (fail "IPv6 address3")
    let spring = replicate len 0
    return $ bs1 ++ spring ++ bs2

ip6 :: Parser IPv6
ip6 = skipSpaces >> toIPv6 <$> ip6arr
  where
    -- appar's <|> does not backtrack, so each branch is wrapped in try.
    ip6arr =
        ip4Embedded
            <|> try
                ( do
                    colon2
                    bs <- option [] hexcolon
                    boundary
                    format [] bs
                )
            <|> try
                ( do
                    rs <- hexcolon
                    boundary
                    check rs
                    return rs
                )
            <|> try
                ( do
                    bs1 <- hexcolon2
                    bs2 <- option [] hexcolon
                    boundary
                    format bs1 bs2
                )
      where
        hexcolon = hex `sepBy1` char ':'
        hexcolon2 = manyTill (hex <* char ':') (char ':')
        check bs = when (length bs /= 8) (fail "IPv6 address4")

ip4Embedded :: Parser [Int]
ip4Embedded =
    try
        ( do
            colon2
            bs <- beforeEmbedded
            embedded <- ip4'
            format [] (bs ++ ip4ToIp6 embedded)
        )
        -- matches 2001:db8::192.0.2.1
        <|> try
            ( do
                bs1 <- manyTill (try $ hex <* char ':') (char ':')
                bs2 <- option [] beforeEmbedded
                embedded <- ip4'
                format bs1 (bs2 ++ ip4ToIp6 embedded)
            )
        -- matches 2001:db8:11e:c00:aa:bb:192.0.2.1
        <|> try
            ( do
                bs <- beforeEmbedded
                embedded <- ip4'
                let rs = bs ++ ip4ToIp6 embedded
                check rs
                return rs
            )
  where
    beforeEmbedded = many $ try $ hex <* char ':'
    ip4ToIp6 [a, b, c, d] =
        [ a `shiftL` 8 .|. b
        , c `shiftL` 8 .|. d
        ]
    ip4ToIp6 _ = error "ip4ToIp6"
    check bs = when (length bs /= 8) (fail "IPv6 address4")

----------------------------------------------------------------
--
-- HostAddress and HostAddress6
--

-- | The 'fromHostAddress' function converts 'HostAddress' to 'IPv4'.
fromHostAddress :: HostAddress -> IPv4
fromHostAddress addr4
    | targetByteOrder == LittleEndian = IP4 $ byteSwap32 addr4
    | otherwise = IP4 addr4

-- | The 'toHostAddress' function converts 'IPv4' to 'HostAddress'.
toHostAddress :: IPv4 -> HostAddress
toHostAddress (IP4 addr4)
    | targetByteOrder == LittleEndian = byteSwap32 addr4
    | otherwise = addr4

-- | The 'fromHostAddress6' function converts 'HostAddress6' to 'IPv6'.
fromHostAddress6 :: HostAddress6 -> IPv6
fromHostAddress6 = IP6

-- | The 'toHostAddress6' function converts 'IPv6' to 'HostAddress6'.
toHostAddress6 :: IPv6 -> HostAddress6
toHostAddress6 (IP6 addr6) = addr6

-- | Convert IPv4 address to IPv4-embedded-in-IPv6
ipv4ToIPv6 :: IPv4 -> IPv6
ipv4ToIPv6 (IP4 w) = IP6 (0, 0, 0xffff, w)

-- | Convert 'SockAddr' to 'IP'.
--
--   Since: 1.7.4.
fromSockAddr :: SockAddr -> Maybe (IP, PortNumber)
fromSockAddr (SockAddrInet pn ha) = Just (IPv4 (fromHostAddress ha), pn)
fromSockAddr (SockAddrInet6 pn _ ha6 _) = Just (IPv6 (fromHostAddress6 ha6), pn)
fromSockAddr _ = Nothing

-- | Convert 'IP' to 'SockAddr'.
--
--   Since: 1.7.8.
toSockAddr :: (IP, PortNumber) -> SockAddr
toSockAddr (IPv4 addr4, pn) = SockAddrInet pn (toHostAddress addr4)
toSockAddr (IPv6 addr6, pn) = SockAddrInet6 pn 0 (toHostAddress6 addr6) 0
