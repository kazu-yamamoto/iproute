{-# LANGUAGE FlexibleInstances #-}
{-|
  Data structures to express IPv4, IPv6 and IP range.
-}
module Data.IP (
    IP(masked, intToMask, intToTBit, isZero)
  , IPv4, toIPv4, isIPv4
  , IPv6, toIPv6, isIPv6
  , IPRange, addr, mask, mlen
  , (>:>), makeIPRange
  ) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Char
import Data.IntMap hiding (map)
import Data.List (foldl')
import Data.Word
import Text.ParserCombinators.Parsec
import Text.Printf

----------------------------------------------------------------
--
-- Data definitions
--

-- This is host byte order
type IPv4Addr = Word32
type IPv6Addr = (Word32,Word32,Word32,Word32)

{-|
  The abstract data structure to express an IPv4 address.
  To create this, use 'toIPv4'. Or use 'read' @\"192.0.2.1\"@ :: 'IPv4', for example.
-}
newtype IPv4 = IPv4 IPv4Addr deriving (Eq, Ord)
{-|
  The abstract data structure to express an IPv6 address.
  To create this, use 'toIPv6'. Or use 'read' @\"2001:DB8::1\"@ :: 'IPv6', for example.
-}
newtype IPv6 = IPv6 IPv6Addr deriving (Eq, Ord)

{-|
  The IP range consists of an 'IP' address, a contiguous 'IP' mask,
  and mask length. The contiguous 'IP' mask and the mask length
  are essentially same information but contained for pre
  calculation.

  To create this, use 'makeIPRange' or 'read' \"192.0.2.0/24\" :: 'IPRange' 'IPv4', for example.
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

data IPVersion = IPVersion4 | IPVersion6 deriving (Eq, Ord)

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
-- Show for IP
--

instance Show IPv4 where
    show = showIPv4

instance Show IPv6 where
    show = showIPv6

showIPv4 :: IPv4 -> String
showIPv4 (IPv4 a) = show4 a
    where
      remQuo x = (x `mod` 256, x `div` 256)
      show4 q = let (a4,q4) = remQuo q
                    (a3,q3) = remQuo q4
                    (a2,q2) = remQuo q3
                    (a1, _) = remQuo q2
                 in printf "%d.%d.%d.%d" a1 a2 a3 a4

showIPv6 :: IPv6 -> String
showIPv6 (IPv6 (a1,a2,a3,a4)) = show6 a1 ++ ":" ++ show6 a2 ++ ":" ++ show6 a3 ++ ":" ++ show6 a4
    where
      remQuo x = (x `mod` 65536, x `div` 65536)
      show6 q = let (r2,q2) = remQuo q
                    (r1, _) = remQuo q2
                in printf "%02x:%02x" r1 r2

----------------------------------------------------------------

isIPv4 :: (IP a) => a -> Bool
isIPv4 x = version x == IPVersion4

isIPv6 :: (IP a) => a -> Bool
isIPv6 x = version x == IPVersion6

----------------------------------------------------------------
--
-- Show for IPRange
--

instance (IP a, Show a) => Show (IPRange a) where
    show x = show (addr x) ++ "/" ++ show (mlen x)

----------------------------------------------------------------
--
-- Mask
--

maskIPv4 :: Int -> IPv4
maskIPv4 len = IPv4 (masksIPv4 ! len)

maskIPv6 :: Int -> IPv6
maskIPv6 len = IPv6 (masksIPv6 ! len)

masksWord32 :: [Word32]
masksWord32 = take 33 $ iterate (flip shift 1) 0xffffffff

masksIPv4 :: IntMap IPv4Addr
masksIPv4 = fromList $ zip [32,31..0] masksWord32

masksIPv6 :: IntMap IPv6Addr
masksIPv6 = fromList $ zip [128,127..0] ms
  where
    ms = m0 ++ m1 ++ m2 ++ m3 ++ m4
    m0 = [(all1,all1,all1,all1)]
    m1 = map (\vmsk -> (all1,all1,all1,vmsk)) masks
    m2 = map (\vmsk -> (all1,all1,vmsk,all0)) masks
    m3 = map (\vmsk -> (all1,vmsk,all0,all0)) masks
    m4 = map (\vmsk -> (vmsk,all0,all0,all0)) masks
    masks = tail masksWord32
    all1 = 0xffffffff
    all0 = 0x00000000

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

----------------------------------------------------------------
--
-- Read for IP
--

instance Read IPv4 where
    readsPrec _ = parseIPv4

instance Read IPv6 where
    readsPrec _ = parseIPv6

parseIPv4 :: String -> [(IPv4,String)]
parseIPv4 cs = case parse (adopt ipv4) "parseIPv4" cs of
                 Right a4 -> a4
                 Left  _  -> error "parseIPv4"

parseIPv6 :: String -> [(IPv6,String)]
parseIPv6 cs = case parse (adopt ipv6) "parseIPv6" cs of
                 Right a6 -> a6
                 Left  _  -> error "parseIPv6"

----------------------------------------------------------------
--
-- Read for IPRange
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

----------------------------------------------------------------
--
-- Adopter for Read
--

adopt :: Parser a -> Parser [(a,String)]
adopt p = do x <- p
             rest <- getInput
             return [(x, rest)]

----------------------------------------------------------------
--
-- IPv4 Parser
--

dig :: Parser Int
dig = do { char '0'; return 0 } <|>
      do n <- oneOf ['1'..'9']
         ns <- many digit
         let ms = map digitToInt (n:ns)
             ret = foldl' (\x y -> x * 10 + y) 0 ms
         return ret

ipv4 :: Parser IPv4
ipv4 = do
    as <- dig `sepBy1` (char '.')
    check as
    return $ toIPv4 as
  where
    test errmsg adr = when (adr < 0 || 255 < adr) (unexpected errmsg)
    check as = do let errmsg = "IPv4 adddress"
                  when (length as /= 4) (unexpected errmsg)
                  mapM_ (test errmsg) as

ipv4range :: Parser (IPRange IPv4)
ipv4range = do
    ip <- ipv4
    len <- option 32 $ do { char '/'; dig }
    check len
    return $ IPRange ip (maskIPv4 len) len
  where
    check len = when (len < 0 || 32 < len) (unexpected "IPv4 mask length")

----------------------------------------------------------------
--
-- IPv6 Parser (RFC 4291)
--

hex :: Parser Int
hex = do ns <- many1 hexDigit
         check ns
         let ms = map digitToInt ns
             val = foldl' (\x y -> x * 16 + y) 0 ms
         return val
    where
      check ns = when (length ns > 4) (unexpected "IPv6 address -- more than 4 hex")

ipv6 :: Parser IPv6
ipv6 = do
    as <- ipv6'
    return $ toIPv6 as

ipv6range :: Parser (IPRange IPv6)
ipv6range = do
    ip <- ipv6
    len <- option 128 $ do { char '/'; dig }
    check len
    return $ IPRange ip (maskIPv6 len) len
  where
    check len = when (len < 0 || 128 < len) (unexpected ("IPv6 mask length: " ++ show len))

ipv6' :: Parser [Int]
ipv6' =     do colon2
               bs <- option [] hexcolon
               rs <- format [] bs
               return rs
        <|> try (do rs <- hexcolon
                    check rs
                    return rs)
        <|> do bs1 <- hexcolon2
               bs2 <- option [] hexcolon
               rs <- format bs1 bs2
               return rs
    where
      colon2 = string "::"
      hexcolon = do bs <- hex `sepBy1` (char ':')
                    return bs
      hexcolon2 = do bs <- manyTill (do{ b <- hex; char ':'; return b }) (char ':')
                     return bs
      format bs1 bs2 = do let len1 = length bs1
                              len2 = length bs2
                          when (len1 > 7) (unexpected "IPv6 address")
                          when (len2 > 7) (unexpected "IPv6 address")
                          let len = 8 - len1 - len2
                          when (len <= 0) (unexpected "IPv6 address")
                          let spring = take len $ repeat 0
                          return $ bs1 ++ spring ++ bs2
      check bs = when (length bs /= 8) (unexpected "IPv6 address")

----------------------------------------------------------------
--
-- IntToIP
--

{-|
  The 'toIPv4' function takes a list of 'Int' and returns 'IPv4'.
  For example, 'toIPv4' @[192,0,2,1]@.
-}
toIPv4 :: [Int] -> IPv4
toIPv4 = IPv4 . toWord32
    where
      toWord32 [a1,a2,a3,a4] = fromIntegral $ shift a1 24 + shift a2 16 + shift a3 8 + a4
      toWord32 _             = error "toWord32"

{-|
  The 'toIPv6' function takes a list of 'Int' and returns 'IPv6'.
  For example, 'toIPv6' @[0x2001,0xDB8,0,0,0,0,0,1]@.
-}
toIPv6 :: [Int] -> IPv6
toIPv6 ad = let [x1,x2,x3,x4] = map toWord32 $ split2 ad
            in IPv6 (x1,x2,x3,x4)
    where
      split2 [] = []
      split2 x  = take 2 x : split2 (drop 2 x)
      toWord32 [a1,a2] = fromIntegral $ shift a1 16 + a2
      toWord32 _             = error "toWord32"
