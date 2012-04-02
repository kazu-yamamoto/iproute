{-# LANGUAGE FlexibleInstances, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

----------------------------------------------------------------
--
-- Tests for IP.RouteTable
--
--    runghc -i.. Test.hs
--

import Control.Monad
import Data.IP
import Data.IP.RouteTable.Internal
import Data.List (sort, nub)
import Prelude hiding (lookup)
import Test.Framework.Providers.DocTest
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH.Prime
import Test.HUnit
import Test.QuickCheck

----------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

----------------------------------------------------------------

doc_test :: DocTests
doc_test = docTest ["../Data/IP.hs"] ["-i.."]

----------------------------------------------------------------
--
-- Arbitrary
--

instance Arbitrary (AddrRange IPv4) where
    arbitrary = arbitraryIP arbitrary 32

instance Arbitrary (AddrRange IPv6) where
    arbitrary = arbitraryIP arbitrary 128

instance Arbitrary IPv4 where
    arbitrary = arbitraryAdr toIPv4 255 4

instance Arbitrary IPv6 where
    arbitrary = arbitraryAdr toIPv6 65535 8

arbitraryAdr :: Routable a => ([Int] -> a) -> Int -> Int -> Gen a
arbitraryAdr func width adrlen = do
    a <- replicateM adrlen (choose (0, width))
    return $ func a

arbitraryIP :: Routable a => Gen a -> Int -> Gen (AddrRange a)
arbitraryIP adrGen msklen = do
    adr <- adrGen
    len <- choose (0,msklen)
    return $ makeAddrRange adr len

----------------------------------------------------------------
--
-- Properties
--

prop_sort_ipv4 :: [AddrRange IPv4] -> Bool
prop_sort_ipv4 = sort_ip

prop_sort_ipv6 :: [AddrRange IPv6] -> Bool
prop_sort_ipv6 = sort_ip

sort_ip :: (Routable a, Ord a) => [AddrRange a] -> Bool
sort_ip xs = fromList (zip xs xs) == fromList (zip xs' xs')
  where
    xs' = sort xs

----------------------------------------------------------------

prop_fromto_ipv4 :: [AddrRange IPv4] -> Bool
prop_fromto_ipv4 = fromto_ip

prop_fromto_ipv6 :: [AddrRange IPv6] -> Bool
prop_fromto_ipv6 = fromto_ip

fromto_ip :: (Routable a, Ord a) => [AddrRange a] -> Bool
fromto_ip xs = nub (sort xs) == nub (sort ys)
  where
   ys = map fst . toList . fromList $ zip xs xs

----------------------------------------------------------------

prop_ord_ipv4 :: [AddrRange IPv4] -> Bool
prop_ord_ipv4 = ord_ip

prop_ord_ipv6 :: [AddrRange IPv6] -> Bool
prop_ord_ipv6 = ord_ip

ord_ip :: Routable a => [AddrRange a] -> Bool
ord_ip xs = isOrdered . fromList $ zip xs xs

isOrdered :: Routable k => IPRTable k a -> Bool
isOrdered = foldt (\x v -> v && ordered x) True

ordered :: Routable k => IPRTable k a -> Bool
ordered Nil = True
ordered (Node k _ _ l r) = ordered' k l && ordered' k r
  where
    ordered' _ Nil = True
    ordered' k1 (Node k2 _ _ _ _) = k1 >:> k2

----------------------------------------------------------------

prop_insert_lookup_ipv4 :: AddrRange IPv4 -> [AddrRange IPv4] -> Bool
prop_insert_lookup_ipv4 = insert_lookup_ip

prop_insert_lookup_ipv6 :: AddrRange IPv6 -> [AddrRange IPv6] -> Bool
prop_insert_lookup_ipv6 = insert_lookup_ip

insert_lookup_ip :: Routable a => AddrRange a -> [AddrRange a] -> Bool
insert_lookup_ip k xs = lookup k (insert k k t) == Just k
  where
    rs = zip xs xs
    t = fromList rs

prop_search_ipv4 :: AddrRange IPv4 -> [AddrRange IPv4] -> Bool
prop_search_ipv4 = search_ip

prop_search_ipv6 :: AddrRange IPv6 -> [AddrRange IPv6] -> Bool
prop_search_ipv6 = search_ip

search_ip :: Routable a => AddrRange a -> [AddrRange a] -> Bool
search_ip k xs = lookup k (fromList (zip xs xs)) == linear k xs

linear :: Routable a => AddrRange a -> [AddrRange a] -> Maybe (AddrRange a)
linear = linear' Nothing
    where
      linear' a _ [] = a
      linear' Nothing k (x:xs)
          | x >:> k   = linear' (Just x) k xs
          | otherwise = linear' Nothing  k xs
      linear' (Just a) k (x:xs)
          | x >:> k   = if mlen x > mlen a
                        then linear' (Just x) k xs
                        else linear' (Just a) k xs
          | otherwise = linear' (Just a) k xs

----------------------------------------------------------------

prop_delete_ipv4 :: [AddrRange IPv6] -> Property
prop_delete_ipv4 = delete_ip

prop_delete_ipv6 :: [AddrRange IPv6] -> Property
prop_delete_ipv6 = delete_ip

delete_ip :: Routable a => [AddrRange a] -> Property
delete_ip xs = xs /= [] ==> isOrdered (delete i t)
  where
    rs = zip xs xs
    t = fromList rs
    i = head xs

prop_insert_delete_ipv4 :: AddrRange IPv4 -> [AddrRange IPv4] -> Property
prop_insert_delete_ipv4 = insert_delete_ip

prop_insert_delete_ipv6 :: AddrRange IPv6 -> [AddrRange IPv6] -> Property
prop_insert_delete_ipv6 = insert_delete_ip

insert_delete_ip :: Routable a => AddrRange a -> [AddrRange a] -> Property
insert_delete_ip k xs = lookup k t == Nothing ==> delete k (insert k k t) == t
  where
    rs = zip xs xs
    t = fromList rs

prop_delete_insert_ipv4 :: [AddrRange IPv4] -> Property
prop_delete_insert_ipv4 = delete_insert_ip

prop_delete_insert_ipv6 :: [AddrRange IPv6] -> Property
prop_delete_insert_ipv6 = delete_insert_ip

delete_insert_ip :: Routable a => [AddrRange a] -> Property
delete_insert_ip xs = xs /= [] ==> insert k k (delete k t) == t
  where
    rs = zip xs xs
    t = fromList rs
    k = head xs

prop_delete_lookup_ipv4 :: [AddrRange IPv4] -> Property
prop_delete_lookup_ipv4 = delete_lookup_ip

prop_delete_lookup_ipv6 :: [AddrRange IPv6] -> Property
prop_delete_lookup_ipv6 = delete_lookup_ip

delete_lookup_ip :: Routable a => [AddrRange a] -> Property
delete_lookup_ip xs = xs /= [] ==> case lookup k (delete k t) of
    Nothing -> True
    Just r  -> r /= k && r >:> k
  where
    rs = zip xs xs
    t = fromList rs
    k = head xs

prop_convert_ipv4 :: IPv4 -> Property
prop_convert_ipv4 ip = True ==> (toIPv4 . fromIPv4) ip == ip

prop_convert_ipv6 :: IPv6 -> Property
prop_convert_ipv6 ip = True ==> (toIPv6 . fromIPv6) ip == ip

----------------------------------------------------------------

ipv4_list :: [AddrRange IPv4]
ipv4_list = [
    "0.0.0.0/0"
  , "133.4.0.0/16"
  , "133.5.0.0/16"
  , "133.5.16.0/24"
  , "133.5.23.0/24"
  ]

case_fromto_ipv4 :: Assertion
case_fromto_ipv4 = (sort . toList . fromList $ pairs) @?= pairs
  where
    pairs = sort $ zip ipv4_list ipv4_list

case_lookup_ipv4 :: Assertion
case_lookup_ipv4 = do
    let rt = fromList pairs
    lookup "127.0.0.1" rt @?= Just "0.0.0.0/0"
    lookup "133.3.0.1" rt @?= Just "0.0.0.0/0"
    lookup "133.4.0.0" rt @?= Just "133.4.0.0/16"
    lookup "133.4.0.1" rt @?= Just "133.4.0.0/16"
    lookup "133.5.16.0" rt @?= Just "133.5.16.0/24"
    lookup "133.5.16.1" rt @?= Just "133.5.16.0/24"
  where
    pairs = zip ipv4_list ipv4_list

case_lookup_ipv4_2 :: Assertion
case_lookup_ipv4_2 = do
    let rt = fromList pairs
    lookup "127.0.0.1" rt @?= Nothing
    lookup "133.3.0.1" rt @?= Nothing
    lookup "133.4.0.0" rt @?= Just "133.4.0.0/16"
    lookup "133.4.0.1" rt @?= Just "133.4.0.0/16"
    lookup "133.5.16.0" rt @?= Just "133.5.16.0/24"
    lookup "133.5.16.1" rt @?= Just "133.5.16.0/24"
  where
    ipv4_list' = tail ipv4_list
    pairs = zip ipv4_list' ipv4_list'

case_findMatch_ipv4 :: Assertion
case_findMatch_ipv4 = do
    let rt = fromList pairs
    findMatch "127.0.0.1" rt @?= []
    findMatch "133.5.23.0/23" rt @?= [("133.5.23.0/24","133.5.23.0/24")]
    findMatch "133.5.0.0/16" rt @?= [
        ("133.5.0.0/16","133.5.0.0/16")
      , ("133.5.16.0/24","133.5.16.0/24")
      , ("133.5.23.0/24","133.5.23.0/24")
      ]
    findMatch "133.4.0.0/16" rt @?= [("133.4.0.0/16", "133.4.0.0/16")]
    findMatch "133.4.0.0/15" rt @?= [
        ("133.4.0.0/16","133.4.0.0/16")
      , ("133.5.0.0/16","133.5.0.0/16")
      , ("133.5.16.0/24","133.5.16.0/24")
      , ("133.5.23.0/24","133.5.23.0/24")
      ]
    findMatch "0.0.0.0/0" rt @?= [
        ("0.0.0.0/0", "0.0.0.0/0")
      , ("133.4.0.0/16", "133.4.0.0/16")
      , ("133.5.0.0/16", "133.5.0.0/16")
      , ("133.5.16.0/24", "133.5.16.0/24")
      , ("133.5.23.0/24", "133.5.23.0/24")
      ]
  where
    pairs = zip ipv4_list ipv4_list

----------------------------------------------------------------