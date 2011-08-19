{-|
  IP routing table is a tree of 'AddrRange'
  to search one of them on the longest
  match base. It is a kind of TRIE with one
  way branching removed. Both IPv4 and IPv6
  are supported.
-}
module Data.IP.RouteTable.Internal where

import Data.Bits
import Data.IP.Addr
import Data.IP.Op
import Data.IP.Range
import Data.IntMap (IntMap, (!))
import qualified Data.IntMap as IM (fromList)
import Data.List (foldl')
import Data.Word
import Prelude hiding (lookup)

----------------------------------------------------------------

{-|
  A class to contain IPv4 and IPv6.
-}
class Addr a => Routable a where
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
    intToTBit = intToTBitIPv4
    isZero a b = a `masked` b == IP4 0

instance Routable IPv6 where
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
intToTBitsWord32 = iterate (`shift` (-1)) 0x80000000

intToTBitsIPv4 :: IntMap IPv4Addr
intToTBitsIPv4 = IM.fromList $ zip [0..32] intToTBitsWord32

intToTBitsIPv6 :: IntMap IPv6Addr
intToTBitsIPv6 = IM.fromList $ zip [0..128] bs
  where
    bs = b1 ++ b2 ++ b3 ++ b4 ++ b5
    b1 = map (\vbit -> (vbit,all0,all0,all0)) intToTBits
    b2 = map (\vbit -> (all0,vbit,all0,all0)) intToTBits
    b3 = map (\vbit -> (all0,all0,vbit,all0)) intToTBits
    b4 = map (\vbit -> (all0,all0,all0,vbit)) intToTBits
    b5 =              [(all0,all0,all0,all0)]
    intToTBits = take 32 intToTBitsWord32
    all0 = 0x00000000

----------------------------------------------------------------

{-|
  The Tree structure for IP routing table based on TRIE with
  one way branching removed. This is an abstracted data structure,
  so you cannot touch its inside. Please use 'insert' or 'lookup', instead.
-}
data IPRTable k a =
    Nil
  | Node !(AddrRange k) !k !(Maybe a) (IPRTable k a) (IPRTable k a)
  deriving (Eq, Show)

----------------------------------------------------------------

{-|
  The 'empty' function returns an empty IP routing table.
-}
empty :: Routable k => IPRTable k a
empty = Nil

----------------------------------------------------------------

{-|
  The 'insert' function inserts a value with a key of 'AddrRange' to 'IPRTable'
  and returns a new 'IPRTable'.
-}
insert :: (Routable k) => AddrRange k -> a -> IPRTable k a -> IPRTable k a
insert k1 v1 Nil = Node k1 tb1 (Just v1) Nil Nil
  where
    tb1 = keyToTestBit k1
insert k1 v1 s@(Node k2 tb2 v2 l r)
    | k1 == k2  = Node k1 tb1 (Just v1) l r
    | k2 >:> k1 = if isLeft k1 tb2
                  then Node k2 tb2 v2 (insert k1 v1 l) r
                  else Node k2 tb2 v2 l (insert k1 v1 r)
    | k1 >:> k2 = if isLeft k2 tb1
                  then Node k1 tb1 (Just v1) s Nil
                  else Node k1 tb1 (Just v1) Nil s
    | otherwise = let n = Node k1 tb1 (Just v1) Nil Nil
                  in join n s
  where
    tb1 = keyToTestBit k1

join :: Routable k => IPRTable k a -> IPRTable k a -> IPRTable k a
join s1@(Node k1 _ _ _ _) s2@(Node k2 _ _ _ _) =
    let kg = glue 0 k1 k2
        tbg = keyToTestBit kg
    in if isLeft k1 tbg
       then Node kg tbg Nothing s1 s2
       else Node kg tbg Nothing s2 s1
join _ _ = error "join"

glue :: (Routable k) => Int -> AddrRange k -> AddrRange k -> AddrRange k
glue n k1 k2
    | addr k1 `masked` mk == addr k2 `masked` mk = glue (n + 1) k1 k2
    | otherwise = makeAddrRange (addr k1) (n - 1)
  where
    mk = intToMask n

keyToTestBit :: Routable k => AddrRange k -> k
keyToTestBit = intToTBit . mlen

isLeft :: Routable k => AddrRange k -> k -> Bool
isLeft adr = isZero (addr adr)

----------------------------------------------------------------

{-|
  The 'delete' function deletes a value by a key of 'AddrRange' from 'IPRTable'
  and returns a new 'IPRTable'.
-}
delete :: (Routable k) => AddrRange k -> IPRTable k a -> IPRTable k a
delete _ Nil = Nil
delete k1 s@(Node k2 tb2 v2 l r)
  | k1 == k2  = node k2 tb2 Nothing l r
  | k2 >:> k1 = if isLeft k1 tb2
                then node k2 tb2 v2 (delete k1 l) r
                else node k2 tb2 v2 l (delete k1 r)
  | otherwise = s

node :: (Routable k) => AddrRange k -> k -> (Maybe a) -> IPRTable k a -> IPRTable k a -> IPRTable k a
node _ _ Nothing Nil r = r
node _ _ Nothing l Nil = l
node k tb v      l   r = Node k tb v l r

----------------------------------------------------------------

{-|
  The 'subLookup' function looks up 'IPRTable' with a key of 'AddrRange'
  and returns True if any of key's subnets are in IPRTable
-}
subLookup :: Routable k => AddrRange k -> IPRTable k a -> Bool
subLookup _ Nil = False
subLookup k1 (Node k2 tb2 Nothing l r)
    | k1 >:> k2 = True
    | k2 >:> k1 = if isLeft k1 tb2
                  then subLookup k1 l
                  else subLookup k1 r
    | otherwise = False
subLookup _ k = True

----------------------------------------------------------------

{-|
  The 'lookup' function looks up 'IPRTable' with a key of 'AddrRange'
  and returns its value if exists.
-}
lookup :: Routable k => AddrRange k -> IPRTable k a -> Maybe a
lookup k s = search k s Nothing

search :: Routable k => AddrRange k -> IPRTable k a -> Maybe a -> Maybe a
search _ Nil res = res
search k1 (Node k2 tb2 Nothing l r) res
    | k1 == k2  = res
    | k2 >:> k1 = if isLeft k1 tb2
                  then search k1 l res
                  else search k1 r res
    | otherwise = res
search k1 (Node k2 tb2 vl l r) res
    | k1 == k2  = vl
    | k2 >:> k1 = if isLeft k1 tb2
                  then search k1 l vl
                  else search k1 r vl
    | otherwise = res

----------------------------------------------------------------

{-|
  The 'fromList' function creates a new IP routing table from
  a list of a pair of 'IPrange' and value.
-}
fromList :: Routable k => [(AddrRange k, a)] -> IPRTable k a
fromList = foldl' (\s (k,v) -> insert k v s) empty

{-|
  The 'toList' function creates a list of a pair of 'AddrRange' and
  value from an IP routing table.
-}
toList :: Routable k => IPRTable k a -> [(AddrRange k, a)]
toList = foldt toL []
  where
    toL Nil xs = xs
    toL (Node _ _ Nothing  _ _) xs = xs
    toL (Node k _ (Just a) _ _) xs = (k,a) : xs

----------------------------------------------------------------

foldt :: (IPRTable k a -> b -> b) -> b -> IPRTable k a -> b
foldt _ v Nil = v
foldt func v rt@(Node _ _ _ l r) = foldt func (foldt func (func rt v) l) r
