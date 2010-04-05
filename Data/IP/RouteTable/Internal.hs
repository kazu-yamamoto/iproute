{-|
  IP routing table is a tree of 'AddrRange'
  to search one of them on the longest
  match base. It is a kind of TRIE with one
  way branching removed. Both IPv4 and IPv6
  are supported.
-}
module Data.IP.RouteTable.Internal where

import Data.IP
import Data.List (foldl')
import Prelude hiding (lookup)

{-|
  The Tree structure for IP routing table based on TRIE with
  one way branching removed. This is an abstracted data structure,
  so you cannot touch its inside. Please use 'insert' or 'lookup', instead.
-}
data IPRTable k a = Nil | Node (Entry k a) (IPRTable k a) (IPRTable k a) deriving (Eq, Show)

data (Routable k) => Entry k a = Entry (AddrRange k) k (Maybe a) deriving (Eq, Show)

----------------------------------------------------------------

{-|
  The 'empty' function returns an empty IP routing table.
-}
empty :: Routable k => IPRTable k a
empty = Nil

----------------------------------------------------------------

newEntry :: (Routable k) => AddrRange k -> Maybe a -> Entry k a
newEntry k v = Entry k (intToTBit (mlen k)) v

{-|
  The 'insert' function inserts a value with a key of 'AddrRange' to 'IPRTable'
  and returns a new 'IPRTable'.
-}
insert :: (Routable k) => AddrRange k -> a -> IPRTable k a -> IPRTable k a
insert k v s = inject (newEntry k (Just v)) s

inject :: (Routable k) => Entry k a -> IPRTable k a -> IPRTable k a
inject e Nil    = Node e Nil Nil
inject e1@(Entry k1 _ _) s@(Node e2@(Entry k2 _ _) l r)
    | k1 == k2  = Node e1 l r
    | k2 >:> k1 = if isLeft k1 e2
                  then Node e2 (inject e1 l) r
                  else Node e2 l (inject e1 r)
    | k1 >:> k2 = if isLeft k2 e1
                  then Node e1 s Nil
                  else Node e1 Nil s
    | otherwise = let n = Node e1 Nil Nil
                  in glue n s

glue :: Routable k => IPRTable k a -> IPRTable k a -> IPRTable k a
glue s1@(Node (Entry k1 _ _) _ _) s2@(Node (Entry k2 _ _) _ _) =
    let kg = makeGlueRange 0 k1 k2
        eg  = newEntry kg Nothing
    in if isLeft k1 eg
       then Node eg s1 s2
       else Node eg s2 s1
glue _ _ = error "glue"

makeGlueRange :: (Routable k) => Int -> AddrRange k -> AddrRange k -> AddrRange k
makeGlueRange n k1 k2
    | addr k1 `masked` mk == addr k2 `masked` mk = makeGlueRange (n + 1) k1 k2

    | otherwise = makeAddrRange (addr k1) (n - 1)
  where
    mk = intToMask n

isLeft :: Routable k => AddrRange k -> Entry k a -> Bool
isLeft adr (Entry _ tb _) = isZero (addr adr) tb

----------------------------------------------------------------

{-|
  The 'lookup' function looks up 'IPRTable' with a key of 'AddrRange'
  and returns its value if exists.
-}
lookup :: Routable k => AddrRange k -> IPRTable k a -> Maybe a
lookup k s = search k s Nothing

search :: Routable k => AddrRange k -> IPRTable k a -> Maybe a -> Maybe a
search _ Nil res = res
search k1 (Node e2@(Entry k2 _ Nothing) l r) res
    | k1 == k2  = res
    | k2 >:> k1 = if isLeft k1 e2
                  then search k1 l res
                  else search k1 r res
    | otherwise = res
search k1 (Node e2@(Entry k2 _ vl) l r) res
    | k1 == k2  = vl
    | k2 >:> k1 = if isLeft k1 e2
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
    toL (Node (Entry _ _ Nothing) _ _) xs = xs
    toL (Node (Entry k _ (Just a)) _ _) xs = (k,a) : xs

----------------------------------------------------------------

foldt :: (IPRTable k a -> b -> b) -> b -> IPRTable k a -> b
foldt _ v Nil = v
foldt func v rt@(Node _ l r) = foldt func (foldt func (func rt v) l) r
