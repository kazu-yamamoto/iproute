{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module BuilderSpec where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Monad
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.IP
import Data.IP.Builder
import Data.IP.RouteTable
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

----------------------------------------------------------------
--
-- Arbitrary
--

b16, b17 :: Int
b16 = 65535
b17 = (b16 + 1) * 2 - 1

instance Arbitrary IPv4 where
    arbitrary = arbitraryAdr toIPv4 255 255 4

-- | Bias the IPv6 generator to produce 0s with ~50% probability, so that we
-- stand a non-trivial chance of testing the gap computation corner cases.
-- We also give 0xffff enhanced odds, by choosing that instead of 0 one
-- time out of 16.
--
instance Arbitrary IPv6 where
    arbitrary = arbitraryAdr toIPv6 b16 b17 8

arbitraryAdr :: Routable a => ([Int] -> a) -> Int -> Int -> Int -> Gen a
arbitraryAdr func width range adrlen =
    func <$> replicateM adrlen biased
  where
    biased = do
        n <- choose(0, range)
        if n <= width
        then return n
        else do
             f <- choose (0, 15 :: Int)
             if f < 15
             then return 0
             else return width

----------------------------------------------------------------
--
-- Spec
--

spec :: Spec
spec = do
    describe "test builders" $ do
        prop "IPv4 Builder matches Show instance" v4_compat
        prop "IPv6 Builder matches Show instance" v6_compat

v4_compat :: IPv4 -> Bool
v4_compat a = builderToString (ipv4Builder a) == show a

v6_compat :: IPv6 -> Bool
v6_compat a = builderToString (ipv6Builder a) == show a

builderToString :: BB.Builder -> String
builderToString = LBSC.unpack . BB.toLazyByteString
