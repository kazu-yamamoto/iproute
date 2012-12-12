{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RouteTableSpec where

import Control.Applicative
import Control.Monad
import Data.IP
import Data.IP.RouteTable.Internal
import Data.List (sort, nub)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

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
arbitraryAdr func width adrlen = func <$> replicateM adrlen (choose (0, width))

arbitraryIP :: Routable a => Gen a -> Int -> Gen (AddrRange a)
arbitraryIP adrGen msklen = makeAddrRange <$> adrGen <*> choose (0,msklen)

----------------------------------------------------------------
--
-- Spec
--

spec :: Spec
spec = do
    describe "fromList" $ do
        prop "creates the same tree for random input and ordered input"
            (sort_ip :: [AddrRange IPv4] -> Bool)
        prop "creates the same tree for random input and ordered input"
            (sort_ip :: [AddrRange IPv6] -> Bool)
        prop "stores input in the incremental order"
            (ord_ip :: [AddrRange IPv4] -> Bool)
        prop "stores input in the incremental order"
            (ord_ip :: [AddrRange IPv6] -> Bool)
    describe "toList" $ do
        prop "expands as sorted"
            (fromto_ip :: [AddrRange IPv4] -> Bool)
        prop "expands as sorted"
            (fromto_ip :: [AddrRange IPv6] -> Bool)

sort_ip :: (Routable a, Ord a) => [AddrRange a] -> Bool
sort_ip xs = fromList (zip xs xs) == fromList (zip xs' xs')
  where
    xs' = sort xs

fromto_ip :: (Routable a, Ord a) => [AddrRange a] -> Bool
fromto_ip xs = nub (sort xs) == nub (sort ys)
  where
   ys = map fst . toList . fromList $ zip xs xs

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
