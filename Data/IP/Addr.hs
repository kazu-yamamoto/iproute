module Data.IP.Addr where

import Control.Monad
import Data.Bits
import Data.Char
import Data.List (foldl')
import Data.String
import Data.Word
import Text.Appar.String
import Text.Printf

----------------------------------------------------------------

{-|
  A unified IP data for 'IPv4' and 'IPv6'.
  To create this, use the data constructors. Or use 'read' @"192.0.2.1"@ :: 'IP', for example. Also, @"192.0.2.1"@ can be used as literal with OverloadedStrings.
-}

data IP = IPv4 { ipv4 :: IPv4 }
        | IPv6 { ipv6 :: IPv6 }
        deriving (Eq)

instance Show IP where
    show (IPv4 ip) = show ip
    show (IPv6 ip) = show ip

----------------------------------------------------------------

-- This is host byte order
type IPv4Addr = Word32
type IPv6Addr = (Word32,Word32,Word32,Word32)

{-|
  The abstract data structure to express an IPv4 address.
  To create this, use 'toIPv4'. Or use 'read' @\"192.0.2.1\"@ :: 'IPv4', for example. Also, @\"192.0.2.1\"@ can be used as literal with OverloadedStrings.
-}
newtype IPv4 = IP4 IPv4Addr deriving (Eq, Ord)

{-|
  The abstract data structure to express an IPv6 address.
  To create this, use 'toIPv6'. Or use 'read' @\"2001:DB8::1\"@ :: 'IPv6', for example. Also, @\"2001:DB8::1\"@ can be used as literal with OverloadedStrings.
-}
newtype IPv6 = IP6 IPv6Addr deriving (Eq, Ord)

----------------------------------------------------------------
--
-- Show
--

instance Show IPv4 where
    show = showIPv4

instance Show IPv6 where
    show = showIPv6

showIPv4 :: IPv4 -> String
showIPv4 (IP4 a) = show4 a
    where
      remQuo x = (x `mod` 256, x `div` 256)
      show4 q = let (a4,q4) = remQuo q
                    (a3,q3) = remQuo q4
                    (a2,q2) = remQuo q3
                    (a1, _) = remQuo q2
                 in printf "%d.%d.%d.%d" a1 a2 a3 a4

showIPv6 :: IPv6 -> String
showIPv6 (IP6 (a1,a2,a3,a4)) = show6 a1 ++ ":" ++ show6 a2 ++ ":" ++ show6 a3 ++ ":" ++ show6 a4
    where
      remQuo x = (x `mod` 65536, x `div` 65536)
      show6 q = let (r2,q2) = remQuo q
                    (r1, _) = remQuo q2
                in printf "%02x:%02x" r1 r2

----------------------------------------------------------------
--
-- IntToIP
--

{-|
  The 'toIPv4' function takes a list of 'Int' and returns 'IPv4'.
  For example, 'toIPv4' @[192,0,2,1]@.
-}
toIPv4 :: [Int] -> IPv4
toIPv4 = IP4 . toWord32
    where
      toWord32 [a1,a2,a3,a4] = fromIntegral $ shift a1 24 + shift a2 16 + shift a3 8 + a4
      toWord32 _             = error "toWord32"

{-|
  The 'toIPv6' function takes a list of 'Int' and returns 'IPv6'.
  For example, 'toIPv6' @[0x2001,0xDB8,0,0,0,0,0,1]@.
-}
toIPv6 :: [Int] -> IPv6
toIPv6 ad = let [x1,x2,x3,x4] = map toWord32 $ split2 ad
            in IP6 (x1,x2,x3,x4)
    where
      split2 [] = []
      split2 x  = take 2 x : split2 (drop 2 x)
      toWord32 [a1,a2] = fromIntegral $ shift a1 16 + a2
      toWord32 _             = error "toWord32"

----------------------------------------------------------------
--
-- IPToInt
--

{-|
  The 'fromIPv4' function convert 'IPv4' to a list of 'Int'.
-}
fromIPv4 :: IPv4 -> [Int]
fromIPv4 (IP4 w) = map (\n -> fromEnum $ (w `shiftR` n) .&. 0xff) [0o30, 0o20, 0o10, 0o00]

{-|
  The 'toIPv6' function convert 'IPv6' to a list of 'Int'.
-}
fromIPv6 :: IPv6 -> [Int]
fromIPv6 (IP6 (w1, w2, w3, w4)) = map fromEnum (concatMap split [w1,w2,w3,w4])
  where
    split :: Word32 -> [Word32]
    split n = [n `shiftR` 0x10 .&. 0xffff, n .&. 0xffff]

----------------------------------------------------------------
--
-- Read
--

instance Read IP where
    readsPrec _ = parseIP

instance Read IPv4 where
    readsPrec _ = parseIPv4

instance Read IPv6 where
    readsPrec _ = parseIPv6

parseIP :: String -> [(IP,String)]
parseIP cs =
  case runParser ip4 cs of
       (Just ip,rest) -> [(IPv4 ip,rest)]
       (Nothing,_) -> case runParser ip6 cs of
                           (Just ip,rest) -> [(IPv6 ip,rest)]
                           (Nothing,_) -> error $ "parseIP" ++ cs

parseIPv4 :: String -> [(IPv4,String)]
parseIPv4 cs = case runParser ip4 cs of
    (Nothing,_)    -> error $ "parseIPv4 " ++ cs
    (Just a4,rest) -> [(a4,rest)]

parseIPv6 :: String -> [(IPv6,String)]
parseIPv6 cs = case runParser ip6 cs of
    (Nothing,_)    -> error $ "parseIPv6 " ++ cs
    (Just a6,rest) -> [(a6,rest)]

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

dig :: Parser Int
dig = 0 <$ char '0'
  <|> toInt <$> oneOf ['1'..'9'] <*> many digit
  where
    toInt n ns = foldl' (\x y -> x * 10 + y) 0 . map digitToInt $ n : ns

ip4 :: Parser IPv4
ip4 = do
    as <- dig `sepBy1` char '.'
    check as
    return $ toIPv4 as
  where
    test errmsg adr = when (adr < 0 || 255 < adr) (fail errmsg)
    check as = do let errmsg = "IPv4 adddress"
                  when (length as /= 4) (fail errmsg)
                  mapM_ (test errmsg) as

----------------------------------------------------------------
--
-- IPv6 Parser (RFC 4291)
--

hex :: Parser Int
hex = do ns <- some hexDigit
         check ns
         let ms = map digitToInt ns
             val = foldl' (\x y -> x * 16 + y) 0 ms
         return val
    where
      check ns = when (length ns > 4) (fail "IPv6 address -- more than 4 hex")

ip6 :: Parser IPv6
ip6 = toIPv6 <$> ip6'

ip6' :: Parser [Int]
ip6' =      do colon2
               bs <- option [] hexcolon
               format [] bs
        <|> try (do rs <- hexcolon
                    check rs
                    return rs)
        <|> do bs1 <- hexcolon2
               bs2 <- option [] hexcolon
               format bs1 bs2
    where
      colon2 = string "::"
      hexcolon = hex `sepBy1` char ':'
      hexcolon2 = manyTill (hex <* char ':') (char ':')
      format bs1 bs2 = do let len1 = length bs1
                              len2 = length bs2
                          when (len1 > 7) (fail "IPv6 address1")
                          when (len2 > 7) (fail "IPv6 address2")
                          let len = 8 - len1 - len2
                          when (len <= 0) (fail "IPv6 address3")
                          let spring = replicate len 0
                          return $ bs1 ++ spring ++ bs2
      check bs = when (length bs /= 8) (fail "IPv6 address4")
