{-# LANGUAGE FlexibleInstances #-}

module Tests where

----------------------------------------------------------------
--
-- Tests for IP.RouteTable
--
--    runghc -i.. Tests.hs
--

import Control.Monad
import Data.IP
import Data.IP.RouteTable.Internal
import Data.List (sort, nub)
import Prelude hiding (lookup)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck

tests :: [Test]
tests = [ testGroup "Property Test" [
               testProperty "Sort IPv4" prop_sort_ipv4
             , testProperty "Sort IPv6" prop_sort_ipv6
             , testProperty "FromTo IPv4" prop_fromto_ipv4
             , testProperty "FromTo IPv6" prop_fromto_ipv6
             , testProperty "Search IPv4" prop_search_ipv4
             , testProperty "Search IPv6" prop_search_ipv6
             , testProperty "Ord IPv4" prop_ord_ipv4
             , testProperty "Ord IPv6" prop_ord_ipv6
             ]
        , testGroup "Test case for routing table" [
               testCase "fromto IPv4" test_fromto_ipv4
             , testCase "lookup IPv4" test_lookup_ipv4
             , testCase "lookup2 IPv4" test_lookup_ipv4_2
             ]
        , testGroup "Test case for IP" [
               testCase "toIPv4" test_toIPv4
             , testCase "toIPv6" test_toIPv6
             , testCase "read IPv4" test_read_IPv4
             , testCase "read IPv6" test_read_IPv6
             , testCase "read IPv6 2" test_read_IPv6_2
             , testCase "read IPv4 range" test_read_IPv4_range
             , testCase "read IPv6 range" test_read_IPv6_range
             , testCase "makeAddrRange IPv4" test_makeAddrRange_IPv4
             , testCase "makeAddrRange IPv6" test_makeAddrRange_IPv6
             , testCase "contains IPv4" test_contains_IPv4
             , testCase "contains IPv4 2" test_contains_IPv4_2
             , testCase "contains IPv6" test_contains_IPv6
             , testCase "contains IPv6 2" test_contains_IPv6_2
             , testCase "isMatchedTo IPv4" test_isMatchedTo_IPv4
             , testCase "isMatchedTo IPv4 2" test_isMatchedTo_IPv4_2
             , testCase "isMatchedTo IPv6" test_isMatchedTo_IPv6
             , testCase "isMatchedTo IPv6 2" test_isMatchedTo_IPv6_2
             ]
        ]

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------
--
-- Arbitrary
--

instance Arbitrary (AddrRange IPv4) where
    arbitrary = arbitraryIP toIPv4 255 4 32

instance Arbitrary (AddrRange IPv6) where
    arbitrary = arbitraryIP toIPv6 65535 8 128

arbitraryIP :: Routable a => ([Int] -> a) -> Int -> Int -> Int -> Gen (AddrRange a)
arbitraryIP func width adrlen msklen = do
  a <- replicateM adrlen (choose (0,width))
  let adr = func a
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
sort_ip xs = fromList (zip xs xs) == let xs' = sort xs
                                     in fromList (zip xs' xs')

----------------------------------------------------------------

prop_fromto_ipv4 :: [AddrRange IPv4] -> Bool
prop_fromto_ipv4 = fromto_ip

prop_fromto_ipv6 :: [AddrRange IPv6] -> Bool
prop_fromto_ipv6 = fromto_ip

fromto_ip :: (Routable a, Ord a) => [AddrRange a] -> Bool
fromto_ip xs = let ys = map fst $ toList $ fromList (zip xs xs)
               in nub (sort xs) == nub (sort ys)

----------------------------------------------------------------

prop_ord_ipv4 :: [AddrRange IPv4] -> Bool
prop_ord_ipv4 = ord_ip

prop_ord_ipv6 :: [AddrRange IPv6] -> Bool
prop_ord_ipv6 = ord_ip

ord_ip :: Routable a => [AddrRange a] -> Bool
ord_ip xs = isOrdered (fromList (zip xs xs))

isOrdered :: Routable k => IPRTable k a -> Bool
isOrdered = foldt (\x v -> v && ordered x) True

ordered :: Routable k => IPRTable k a -> Bool
ordered Nil = True
ordered (Node k _ _ l r) = ordered' k l && ordered' k r
  where
    ordered' _ Nil = True
    ordered' k1 (Node k2 _ _ _ _) = k1 >:> k2

----------------------------------------------------------------

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

test_toIPv4 :: Assertion
test_toIPv4 = show (toIPv4 [192,0,2,1]) @?= "192.0.2.1"

test_toIPv6 :: Assertion
test_toIPv6 = show (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) @?= "2001:db8:00:00:00:00:00:01"

test_read_IPv4 :: Assertion
test_read_IPv4 = show (read "192.0.2.1" :: IPv4) @?= "192.0.2.1"

test_read_IPv6 :: Assertion
test_read_IPv6 = show (read "2001:db8:00:00:00:00:00:01" :: IPv6) @?= "2001:db8:00:00:00:00:00:01"

test_read_IPv6_2 :: Assertion
test_read_IPv6_2 = show (read "2001:240:11e:c00::101" :: IPv6) @?= "2001:240:11e:c00:00:00:00:101"

test_read_IPv4_range :: Assertion
test_read_IPv4_range = show (read "192.0.2.1/24" :: AddrRange IPv4) @?= "192.0.2.0/24"

test_read_IPv6_range :: Assertion
test_read_IPv6_range = show (read "2001:db8:00:00:00:00:00:01/48" :: AddrRange IPv6) @?= "2001:db8:00:00:00:00:00:00/48"

----------------------------------------------------------------

test_makeAddrRange_IPv4 :: Assertion
test_makeAddrRange_IPv4 = show (makeAddrRange (toIPv4 [127,0,2,1]) 8) @?= "127.0.0.0/8"

test_makeAddrRange_IPv6 :: Assertion
test_makeAddrRange_IPv6 = show (makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 8) @?= "2000:00:00:00:00:00:00:00/8"

----------------------------------------------------------------

test_contains_IPv4 :: Assertion
test_contains_IPv4 = makeAddrRange (toIPv4 [127,0,2,1]) 8 >:> makeAddrRange (toIPv4 [127,0,2,1]) 24 @?= True

test_contains_IPv4_2 :: Assertion
test_contains_IPv4_2 = makeAddrRange (toIPv4 [127,0,2,1]) 24 >:> makeAddrRange (toIPv4 [127,0,2,1]) 8 @?= False

test_contains_IPv6 :: Assertion
test_contains_IPv6 = makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 16 >:> makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 32 @?= True

test_contains_IPv6_2 :: Assertion
test_contains_IPv6_2 = makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 32 >:> makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 16 @?= False

----------------------------------------------------------------

test_isMatchedTo_IPv4 :: Assertion
test_isMatchedTo_IPv4 = toIPv4 [127,0,2,1] `isMatchedTo` makeAddrRange (toIPv4 [127,0,2,1]) 24 @?= True

test_isMatchedTo_IPv4_2 :: Assertion
test_isMatchedTo_IPv4_2 = toIPv4 [127,0,2,0] `isMatchedTo` makeAddrRange (toIPv4 [127,0,2,1]) 32 @?= False

test_isMatchedTo_IPv6 :: Assertion
test_isMatchedTo_IPv6 = toIPv6 [0x2001,0xDB8,0,0,0,0,0,1] `isMatchedTo` makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 32 @?= True

test_isMatchedTo_IPv6_2 :: Assertion
test_isMatchedTo_IPv6_2 = toIPv6 [0x2001,0xDB8,0,0,0,0,0,0] `isMatchedTo` makeAddrRange (toIPv6 [0x2001,0xDB8,0,0,0,0,0,1]) 128 @?= False

----------------------------------------------------------------

ipv4_list :: [AddrRange IPv4]
ipv4_list = [
    read "0.0.0.0/0"
  , read "133.4.0.0/16"
  , read "133.5.0.0/16"
  , read "133.5.16.0/24"
  , read "133.5.23.0/24"
  ]

test_fromto_ipv4 :: Assertion
test_fromto_ipv4 = (sort . toList . fromList $ pairs) @?= pairs
  where
    pairs = sort $ zip ipv4_list ipv4_list

test_lookup_ipv4 :: Assertion
test_lookup_ipv4 = do
    let rt = fromList pairs
    lookup (read "127.0.0.1") rt @?= Just (read "0.0.0.0/0")
    lookup (read "133.3.0.1") rt @?= Just (read "0.0.0.0/0")
    lookup (read "133.4.0.0") rt @?= Just (read "133.4.0.0/16")
    lookup (read "133.4.0.1") rt @?= Just (read "133.4.0.0/16")
    lookup (read "133.5.16.0") rt @?= Just (read "133.5.16.0/24")
    lookup (read "133.5.16.1") rt @?= Just (read "133.5.16.0/24")
  where
    pairs = zip ipv4_list ipv4_list

test_lookup_ipv4_2 :: Assertion
test_lookup_ipv4_2 = do
    let rt = fromList pairs
    lookup (read "127.0.0.1") rt @?= Nothing
    lookup (read "133.3.0.1") rt @?= Nothing
    lookup (read "133.4.0.0") rt @?= Just (read "133.4.0.0/16")
    lookup (read "133.4.0.1") rt @?= Just (read "133.4.0.0/16")
    lookup (read "133.5.16.0") rt @?= Just (read "133.5.16.0/24")
    lookup (read "133.5.16.1") rt @?= Just (read "133.5.16.0/24")
  where
    ipv4_list' = tail ipv4_list
    pairs = zip ipv4_list' ipv4_list'
