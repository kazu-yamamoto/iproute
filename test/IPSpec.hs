{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module IPSpec where

import Control.Exception (evaluate)
import Data.IP
import Data.Maybe (isJust, isNothing)
import Safe (readMay)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import RouteTableSpec ()

----------------------------------------------------------------
--
-- Arbitrary
--

newtype InvalidIPv4Str = Iv4 String deriving (Show)

instance Arbitrary InvalidIPv4Str where
    arbitrary =
        frequency
            [ (8, arbitraryIIPv4Str arbitrary 32)
            , -- an IPv4 address should not end with a trailing `.`
              (1, Iv4 . (++ ".") . show <$> genIPv4)
            , -- an IPv4 address with mask should not include a white space

                ( 1
                , (\ip (NonNegative len) -> Iv4 (show ip ++ " /" ++ show (len :: Integer)))
                    <$> genIPv4
                    <*> arbitrary
                )
            ]
      where
        genIPv4 :: Gen IPv4
        genIPv4 = arbitrary

arbitraryIIPv4Str :: Gen IPv4 -> Int -> Gen InvalidIPv4Str
arbitraryIIPv4Str adrGen msklen = toIv4 <$> adrGen <*> lenGen
  where
    toIv4 adr len = Iv4 $ show adr ++ "/" ++ show len
    lenGen = oneof [choose (minBound, -1), choose (msklen + 1, maxBound)]

newtype InvalidIPv6Str = Iv6 String deriving (Show)

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
        it "does not read overflow IPv4 octets" $ do
            (readMay "127.0.0.18446744073709551617" :: Maybe IPv4) `shouldBe` Nothing
        it "can read even if unnecessary spaces exist" $ do
            (readMay " ::1" :: Maybe IPv4) `shouldBe` readMay "::1"
        it "does not read overflow mask lengths" $ do
            (readMay "192.168.0.1/18446744073709551648" :: Maybe (AddrRange IPv4))
                `shouldBe` Nothing
        it "can read embedded v4 in v6 range" $ do
            (readMay "::ffff:192.0.2.0/120" :: Maybe (AddrRange IPv6))
                `shouldSatisfy` isJust
        prop "lists of IP" $ \xs ys ->
            let ips = map IPv4 xs ++ map IPv6 ys in readMay (show ips) == Just ips
        prop "pairs of IPv4 and Int" $ \x n ->
            readMay (show (x :: IPv4, n :: Int)) == Just (x, n)
        prop "pairs of IPv6 and Int" $ \x n ->
            readMay (show (x :: IPv6, n :: Int)) == Just (x, n)
        prop "lists of IPRange" $ \xs ys ->
            let rs = map IPv4Range xs ++ map IPv6Range ys
             in readMay (show rs) == Just rs
        it "returns the rest of the input" $ do
            (reads "192.0.2.1 x" :: [(IPv4, String)])
                `shouldBe` [(read "192.0.2.1", " x")]
            (reads "::192.0.2.1 x" :: [(IPv6, String)])
                `shouldBe` [(read "::192.0.2.1", " x")]
            (reads "192.0.2.0/24 x" :: [(AddrRange IPv4, String)])
                `shouldBe` [(read "192.0.2.0/24", " x")]
        it "does not stop in the middle of an address" $ do
            (reads "192.0.2.1a" :: [(IPv4, String)]) `shouldBe` []
            (reads "1:2:3:4:5:6:7:8:9" :: [(IPv6, String)]) `shouldBe` []

    describe "makeAddrRange" $ do
        it "clamps the mask length" $ do
            makeAddrRange ("192.0.2.1" :: IPv4) 40 `shouldBe` "192.0.2.1/32"
            makeAddrRange ("192.0.2.1" :: IPv4) (-1) `shouldBe` "0.0.0.0/0"
            makeAddrRange ("2001:db8::1" :: IPv6) 200 `shouldBe` "2001:db8::1/128"
            makeAddrRange ("2001:db8::1" :: IPv6) (-1) `shouldBe` "::/0"
        prop "makes a valid IPv4 range" $ \a n ->
            let r = makeAddrRange (a :: IPv4) n in readMay (show r) == Just r
        prop "makes a valid IPv6 range" $ \a n ->
            let r = makeAddrRange (a :: IPv6) n in readMay (show r) == Just r
    describe "IPv4-mapped IPv6" $ do
        prop "IPRange compares like IP" $ \x y ->
            let r4 = IPv4Range x
                r6 = IPv6Range (ipv4RangeToIPv6 y)
             in (r4 == r6) == (x == y) && compare r4 r6 == compare x y
        it "enumerates mixed families as IPv6" $ do
            [IPv4 "192.0.2.1" .. IPv6 "::ffff:192.0.2.2"]
                `shouldBe` [IPv6 "::ffff:192.0.2.1", IPv6 "::ffff:192.0.2.2"]
    describe "enum" $ do
        it "stops at maxBound" $ do
            [maxBound ..] `shouldBe` [maxBound :: IPv4]
            [maxBound ..] `shouldBe` [maxBound :: IPv6]
            [pred maxBound, maxBound ..] `shouldBe` [pred maxBound, maxBound :: IPv4]
            [pred maxBound, maxBound ..] `shouldBe` [pred maxBound, maxBound :: IPv6]
        it "stops at minBound" $ do
            [succ minBound, minBound ..] `shouldBe` [succ minBound, minBound :: IPv4]
            [succ minBound, minBound ..] `shouldBe` [succ minBound, minBound :: IPv6]
        it "returns [] for an empty range" $ do
            ["::2" .. "::1"] `shouldBe` ([] :: [IPv6])
            ["0.0.0.2" .. "0.0.0.1"] `shouldBe` ([] :: [IPv4])
        it "converts IPv6 to Int and back" $ do
            fromEnum ("::1:0" :: IPv6) `shouldBe` 0x10000
            toEnum 0x10000 `shouldBe` ("::1:0" :: IPv6)
        it "rejects IPv6 which does not fit in Int" $ do
            evaluate (fromEnum ("1::" :: IPv6)) `shouldThrow` anyErrorCall
            evaluate (toEnum (-1) :: IPv6) `shouldThrow` anyErrorCall
        prop "IPv6 enumFromTo is consistent with Integer" $ \x (Small n) ->
            let y = toEnum (fromEnum (x :: IPv4) `mod` 0x10000) :: IPv6
                z = toEnum (fromEnum y + (abs n :: Int)) :: IPv6
             in map fromEnum [y .. z] == [fromEnum y .. fromEnum z]

to_str_ipv4 :: AddrRange IPv4 -> Bool
to_str_ipv4 a = readMay (show a) == Just a

to_str_ipv6 :: AddrRange IPv6 -> Bool
to_str_ipv6 a = readMay (show a) == Just a

ipv4_fail :: InvalidIPv4Str -> Bool
ipv4_fail (Iv4 a) = isNothing (readMay a :: Maybe (AddrRange IPv4))

ipv6_fail :: InvalidIPv6Str -> Bool
ipv6_fail (Iv6 a) = isNothing (readMay a :: Maybe (AddrRange IPv6))
