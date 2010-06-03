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
import Test.QuickCheck

{-
options :: TestOptions
options = TestOptions { no_of_tests     = 300
                      , length_of_tests = 0
                      , debug_tests     = False
                      }

main :: IO ()
main =
  runTests "base" options [ run prop_sort_ipv4
                          , run prop_sort_ipv6
                          , run prop_fromto_ipv4
                          , run prop_fromto_ipv6
                          , run prop_search_ipv4
                          , run prop_search_ipv6
                          , run prop_ord_ipv4
                          , run prop_ord_ipv6
                          ]
-}

main :: IO ()
main = quickCheckWith stdArgs {maxSuccess = 300} prop_search_ipv4

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
ordered (Node n l r) = ordered' n l && ordered' n r
  where
    ordered' _ Nil = True
    ordered' (Entry k1 _ _) (Node (Entry k2 _ _) _ _) = k1 >:> k2

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
