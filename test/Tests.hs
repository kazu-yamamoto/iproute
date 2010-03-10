{-# LANGUAGE FlexibleInstances #-}
----------------------------------------------------------------
-- 
-- Tests for IP.RouteTable
-- 
--    runghc -i.. Tests.hs
--

import Data.IP
import Data.IP.RouteTable.Internal
import Test.QuickCheck hiding (check, test)
import Test.QuickCheck.Batch
import Prelude hiding (lookup)
import Data.List (sort, nub)

options :: TestOptions
options = TestOptions { no_of_tests     = 300
                      , length_of_tests = 0
                      , debug_tests     = False
                      }

main :: IO ()
main = do
  runTests "base" options [ run prop_sort_ipv4
                          , run prop_sort_ipv6
                          , run prop_fromto_ipv4
                          , run prop_fromto_ipv6
                          , run prop_search_ipv4
                          , run prop_search_ipv6
                          , run prop_ord_ipv4
                          , run prop_ord_ipv6
                          ]

----------------------------------------------------------------
--
-- Arbitrary
--

instance Arbitrary (IPRange IPv4) where
    arbitrary = arbitraryIP toIPv4 255 4 32
    coarbitrary = undefined

instance Arbitrary (IPRange IPv6) where
    arbitrary = arbitraryIP toIPv6 65535 8 128
    coarbitrary = error "coarbitrary"

arbitraryIP :: IP a => ([Int] -> a) -> Int -> Int -> Int -> Gen (IPRange a)
arbitraryIP func width adrlen msklen = do
  a <- sequence $ take adrlen $ repeat (choose (0,width))
  let adr = func a
  len <- choose (0,msklen)
  return $ makeIPRange adr len

----------------------------------------------------------------
--
-- Properties
--

prop_sort_ipv4 :: [IPRange IPv4] -> Bool
prop_sort_ipv4 = sort_ip

prop_sort_ipv6 :: [IPRange IPv6] -> Bool
prop_sort_ipv6 = sort_ip

sort_ip :: (IP a, Ord a) => [IPRange a] -> Bool
sort_ip xs = fromList (zip xs xs) == let xs' = sort xs
                                     in fromList (zip xs' xs')

----------------------------------------------------------------

prop_fromto_ipv4 :: [IPRange IPv4] -> Bool
prop_fromto_ipv4 = fromto_ip

prop_fromto_ipv6 :: [IPRange IPv6] -> Bool
prop_fromto_ipv6 = fromto_ip

fromto_ip :: (IP a, Ord a) => [IPRange a] -> Bool
fromto_ip xs = let ys = map fst $ toList $ fromList (zip xs xs)
               in nub (sort xs) == nub (sort ys)

----------------------------------------------------------------

prop_ord_ipv4 :: [IPRange IPv4] -> Bool
prop_ord_ipv4 = ord_ip

prop_ord_ipv6 :: [IPRange IPv6] -> Bool
prop_ord_ipv6 = ord_ip

ord_ip :: IP a => [IPRange a] -> Bool
ord_ip xs = isOrdered (fromList (zip xs xs))

isOrdered :: IP k => IPRTable k a -> Bool
isOrdered = foldt (\x v -> v && ordered x) True

ordered :: IP k => IPRTable k a -> Bool
ordered Nil = True
ordered (Node n l r) = ordered' n l && ordered' n r
  where
    ordered' _ Nil = True
    ordered' (Entry k1 _ _) (Node (Entry k2 _ _) _ _) = k1 >:> k2

----------------------------------------------------------------

prop_search_ipv4 :: IPRange IPv4 -> [IPRange IPv4] -> Bool
prop_search_ipv4 = search_ip

prop_search_ipv6 :: IPRange IPv6 -> [IPRange IPv6] -> Bool
prop_search_ipv6 = search_ip

search_ip :: IP a => IPRange a -> [IPRange a] -> Bool
search_ip k xs = lookup k (fromList (zip xs xs)) == linear k xs

linear :: IP a => IPRange a -> [IPRange a] -> Maybe (IPRange a)
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
