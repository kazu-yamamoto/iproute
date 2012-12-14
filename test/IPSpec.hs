{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IPSpec where

import Control.Applicative
import Data.IP
import Safe (readMay)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import RouteTableSpec ()

----------------------------------------------------------------
--
-- Arbitrary
--

data InvalidIPv4Str = Iv4 String deriving (Show)

instance Arbitrary InvalidIPv4Str where
    arbitrary = arbitraryIIPv4Str arbitrary 32

arbitraryIIPv4Str :: Gen IPv4 -> Int -> Gen InvalidIPv4Str
arbitraryIIPv4Str adrGen msklen = toIv4 <$> adrGen <*> lenGen
  where
    toIv4 adr len = Iv4 $ show adr ++ "/" ++ show len
    lenGen = oneof [choose (minBound, -1), choose (msklen + 1, maxBound)]

data InvalidIPv6Str = Iv6 String deriving (Show)

instance Arbitrary InvalidIPv6Str where
    arbitrary = arbitraryIIPv6Str arbitrary 128

arbitraryIIPv6Str :: Gen IPv6 -> Int -> Gen InvalidIPv6Str
arbitraryIIPv6Str adrGen msklen = toIv6 <$> adrGen <*> lenGen
  where
    toIv6 adr len = Iv6 $ show adr ++ "/" ++ show len
    lenGen = oneof [choose (minBound, -1), choose (msklen + 1, maxBound)]

----------------------------------------------------------------
--
-- Spec
--

spec :: Spec
spec = do
    describe "read" $ do
        prop "IPv4" to_str_ipv4
        prop "IPv6" to_str_ipv6
        prop "IPv4 failure" ipv4_fail
        prop "IPv6 failure" ipv6_fail
        it "can read even if unnecessary spaces exist" $ do
            (readMay " 127.0.0.1" :: Maybe IPv4) `shouldBe` readMay "127.0.0.1"
        it "can read even if unnecessary spaces exist" $ do
            (readMay " ::1" :: Maybe IPv4) `shouldBe` readMay "::1"

to_str_ipv4 :: AddrRange IPv4 -> Bool
to_str_ipv4 a = readMay (show a) == Just a

to_str_ipv6 :: AddrRange IPv6 -> Bool
to_str_ipv6 a = readMay (show a) == Just a

ipv4_fail :: InvalidIPv4Str -> Bool
ipv4_fail (Iv4 a) = (readMay a :: Maybe (AddrRange IPv4)) == Nothing

ipv6_fail :: InvalidIPv6Str -> Bool
ipv6_fail (Iv6 a) = (readMay a :: Maybe (AddrRange IPv6)) == Nothing
