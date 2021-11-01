{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoStrict #-}
{-# LANGUAGE TupleSections #-}

module Data.IP.Builder
    ( -- * 'P.BoundedPrim' 'B.Builder's for general, IPv4 and IPv6 addresses.
      ipBuilder
    , ipv4Builder
    , ipv6Builder
    ) where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Prim as P
import           Data.ByteString.Builder.Prim ((>$<), (>*<))
import           GHC.Exts
import           GHC.Word (Word8(..), Word16(..), Word32(..))

import           Data.IP.Addr

------------ IP builders

{-# INLINE ipBuilder #-}
-- | 'P.BoundedPrim' bytestring 'B.Builder' for general 'IP' addresses.
ipBuilder :: IP -> B.Builder
ipBuilder (IPv4 addr) = ipv4Builder addr
ipBuilder (IPv6 addr) = ipv6Builder addr

{-# INLINE ipv4Builder #-}
-- | 'P.BoundedPrim' bytestring 'B.Builder' for 'IPv4' addresses.
ipv4Builder :: IPv4 -> B.Builder
ipv4Builder addr = P.primBounded ipv4Bounded $! fromIPv4w addr

{-# INLINE ipv6Builder #-}
-- | 'P.BoundedPrim' bytestring 'B.Builder' for 'IPv6' addresses.
ipv6Builder :: IPv6 -> B.Builder
ipv6Builder addr = P.primBounded ipv6Bounded $! fromIPv6w addr

------------ Builder utilities

-- Convert fixed to bounded for fusion
toB :: P.FixedPrim a -> P.BoundedPrim a
toB = P.liftFixedToBounded
{-# INLINE toB #-}

ipv4Bounded :: P.BoundedPrim Word32
ipv4Bounded =
    quads >$< ((P.word8Dec >*< dotsep) >*< (P.word8Dec >*< dotsep))
          >*< ((P.word8Dec >*< dotsep) >*< P.word8Dec)
  where
    quads a = ((qdot 0o30# a, qdot 0o20# a), (qdot 0o10# a, qfin a))
    {-# INLINE quads #-}
    qdot s (W32# a) = (W8# (wordToWord8Compat# ((word32ToWordCompat# a `uncheckedShiftRL#` s) `and#` 0xff##)), ())
    {-# INLINE qdot #-}
    qfin (W32# a) = W8# (wordToWord8Compat# (word32ToWordCompat# a `and#` 0xff##))
    {-# INLINE qfin #-}
    dotsep = const 0x2e >$< toB P.word8

-- | For each 32-bit chunk of an IPv6 address, encode its display format in the
-- presentation form of the address, based on its location relative to the
-- "best gap", i.e. the left-most longest run of zeros. The "hi" (H) and/or
-- "lo" (L) 16 bits may be accompanied by colons (C) on the left and/or right.
--
data FF = CHL Word32  -- ^ :<h>:<l>
        | HL  Word32  -- ^  <h>:<l>
        | NOP         -- ^  nop
        | COL         -- ^ :
        | CC          -- ^ :   :
        | CLO Word32  -- ^     :<l>
        | CHC Word32  -- ^ :<h>:
        | HC  Word32  -- ^  <h>:

-- Build an IPv6 address in conformance with
-- [RFC5952](http://tools.ietf.org/html/rfc5952 RFC 5952).
--
ipv6Bounded :: P.BoundedPrim (Word32, Word32, Word32, Word32)
ipv6Bounded =
    P.condB generalCase
      ( genFields >$< output128 )
      ( P.condB v4mapped
          ( pairPair >$< (colsep >*< colsep)
                     >*< (ffff >*< (fstUnit >$< colsep >*< ipv4Bounded)) )
          ( pairPair >$< (P.emptyB >*< colsep) >*< (colsep >*< ipv4Bounded) ) )
  where
    -- The boundedPrim switches and predicates need to be inlined for best
    -- performance, gaining a factor of ~2 in throughput in tests.
    --
    {-# INLINE output128 #-}
    {-# INLINE output64 #-}
    {-# INLINE generalCase #-}
    {-# INLINE v4mapped #-}
    {-# INLINE output32 #-}

    generalCase :: (Word32, Word32, Word32, Word32) -> Bool
    generalCase (w0, w1, w2, w3) =
        w0 /= 0 || w1 /= 0 || (w2 /= 0xffff && (w2 /= 0 || w3 <= 0xffff))
    --
    v4mapped :: (Word32, Word32, Word32, Word32) -> Bool
    v4mapped (w0, w1, w2, _) =
        w0 == 0 && w1 == 0 && w2 == 0xffff

    -- BoundedPrim for the full 128-bit IPv6 address given as
    -- a pair of pairs of FF values, which encode the
    -- output format of each of the 32-bit chunks.
    --
    output128 :: P.BoundedPrim ((FF, FF), (FF, FF))
    output128 = output64 >*< output64
    output64 = (output32 >*< output32)
    --
    -- And finally the per-word case-work.
    --
    output32 :: P.BoundedPrim FF
    output32 =
        P.condB (\case { CHL _ -> True; _ -> False }) build_CHL $ -- :hi:lo
        P.condB (\case { HL _  -> True; _ -> False }) build_HL  $ --  hi:lo
        P.condB (\case { NOP   -> True; _ -> False }) build_NOP $ --
        P.condB (\case { COL   -> True; _ -> False }) build_COL $ -- :
        P.condB (\case { CC    -> True; _ -> False }) build_CC  $ -- :  :
        P.condB (\case { CLO _ -> True; _ -> False }) build_CLO $ --    :lo
        P.condB (\case { CHC _ -> True; _ -> False }) build_CHC $ -- :hi:
                                                      build_HC    --  hi:

    -- encoders for the eight field format (FF) cases.
    --
    build_CHL = ( \ case CHL w -> ( fstUnit (hi16 w), fstUnit (lo16 w) )
                         _     -> undefined )
                >$< (colsep >*< P.word16Hex)
                >*< (colsep >*< P.word16Hex)
    --
    build_HL  = ( \ case HL  w -> ( hi16 w, fstUnit (lo16 w) )
                         _     -> undefined )
                >$< P.word16Hex >*< colsep >*< P.word16Hex
    --
    build_NOP  = P.emptyB
    --
    build_COL  = const () >$< colsep
    --
    build_CC   = const ((), ()) >$< colsep >*< colsep
    --
    build_CLO = ( \ case CLO w -> fstUnit (lo16 w)
                         _     -> undefined )
                >$< colsep >*< P.word16Hex
    --
    build_CHC = ( \ case CHC w -> fstUnit (sndUnit (hi16 w))
                         _     -> undefined )
                >$< colsep >*< P.word16Hex >*< colsep
    --
    build_HC  = ( \ case HC  w -> sndUnit (hi16 w)
                         _     -> undefined )
                >$< P.word16Hex >*< colsep

    -- static encoders
    --
    colsep :: P.BoundedPrim a
    colsep = toB $ const 0x3a >$< P.word8
    --
    ffff :: P.BoundedPrim a
    ffff = toB $ const 0xffff >$< P.word16HexFixed

    -- | Helpers
    hi16, lo16 :: Word32 -> Word16
    hi16 !(W32# w) = W16# (wordToWord16Compat# (word32ToWordCompat# w `uncheckedShiftRL#` 16#))
    lo16 !(W32# w) = W16# (wordToWord16Compat# (word32ToWordCompat# w `and#` 0xffff##))
    --
    fstUnit :: a -> ((), a)
    fstUnit = ((), )
    --
    sndUnit :: a -> (a, ())
    sndUnit = (, ())
    --
    pairPair (a, b, c, d) = ((a, b), (c, d))

    -- Construct fields decorated with output format details
    genFields (w0, w1, w2, w3) =
        let !(!gapStart, !gapEnd) = bestgap w0 w1 w2 w3
            !f0 = makeF0 gapStart gapEnd w0
            !f1 = makeF12 gapStart gapEnd 2# 3# w1
            !f2 = makeF12 gapStart gapEnd 4# 5# w2
            !f3 = makeF3 gapStart gapEnd w3
         in ((f0, f1), (f2, f3))

    makeF0 (I# gapStart) (I# gapEnd) !w =
        case (gapEnd ==# 0#) `orI#` (gapStart ># 1#) of
        1#                               -> HL  w
        _  -> case gapStart ==# 0# of
              1#                         -> COL
              _                          -> HC  w
    {-# INLINE makeF0 #-}

    makeF12 (I# gapStart) (I# gapEnd) il ir !w =
        case (gapEnd <=# il) `orI#` (gapStart ># ir) of
        1#                               -> CHL w
        _ -> case gapStart >=# il of
             1# -> case gapStart ==# il of
                   1#                    -> COL
                   _                     -> CHC w
             _  -> case gapEnd ==# ir of
                   0#                    -> NOP
                   _                     -> CLO w
    {-# INLINE makeF12 #-}

    makeF3 (I# gapStart) (I# gapEnd) !w =
        case gapEnd <=# 6# of
        1#                               -> CHL w
        _ -> case gapStart ==# 6# of
             0# -> case gapEnd ==# 8# of
                   1#                    -> COL
                   _                     -> CLO w
             _                           -> CC
    {-# INLINE makeF3 #-}

-- | Unrolled and inlined calculation of the first longest
-- run (gap) of 16-bit aligned zeros in the input address.
--
bestgap :: Word32 -> Word32 -> Word32 -> Word32 -> (Int, Int)
bestgap !(W32# a0) !(W32# a1) !(W32# a2) !(W32# a3) =
    finalGap
        (updateGap (0xffff##     `and#` (word32ToWordCompat# a3))
        (updateGap (0xffff0000## `and#` (word32ToWordCompat# a3))
        (updateGap (0xffff##     `and#` (word32ToWordCompat# a2))
        (updateGap (0xffff0000## `and#` (word32ToWordCompat# a2))
        (updateGap (0xffff##     `and#` (word32ToWordCompat# a1))
        (updateGap (0xffff0000## `and#` (word32ToWordCompat# a1))
        (updateGap (0xffff##     `and#` (word32ToWordCompat# a0))
        (initGap   (0xffff0000## `and#` (word32ToWordCompat# a0))))))))))
  where

    -- The state after the first input word is always i' = 7,
    -- but if the input word is zero, then also g=z=1 and e'=7.
    initGap :: Word# -> Int#
    initGap w = case w of { 0## -> 0x1717#; _ -> 0x0707# }

    -- Update the nibbles of g|e'|z|i' based on the next input
    -- word.  We always decrement i', reset z on non-zero input,
    -- otherwise increment z and check for a new best gap, if so
    -- we replace g|e' with z|i'.
    updateGap :: Word# -> Int# -> Int#
    updateGap w g = case w `neWord#` 0## of
        1# -> (g +# 0xffff#) `andI#` 0xff0f#  -- g, e, 0, --i
        _  -> let old = g +# 0xf#             -- ++z, --i
                  zi  = old `andI#` 0xff#
                  new = (zi `uncheckedIShiftL#` 8#) `orI#` zi
               in case new ># old of
                  1# -> new            -- z, i, z, i
                  _  -> old            -- g, e, z, i

    -- Extract gap start and end from the nibbles of g|e'|z|i'
    -- where g is the gap width and e' is 8 minus its end.
    finalGap :: Int# -> (Int, Int)
    finalGap i =
        let g = i `uncheckedIShiftRL#` 12#
         in case g <# 2# of
            1# -> (0, 0)
            _  -> let e = 8# -# ((i `uncheckedIShiftRL#` 8#) `andI#` 0xf#)
                      s = e -# g
                   in (I# s, I# e)
{-# INLINE bestgap #-}

#if MIN_VERSION_base(4,16,0)
word32ToWordCompat# :: Word32# -> Word#
word32ToWordCompat# = word32ToWord#

wordToWord8Compat# :: Word# -> Word8#
wordToWord8Compat# = wordToWord8#

wordToWord16Compat# :: Word# -> Word16#
wordToWord16Compat# = wordToWord16#
#else
word32ToWordCompat# :: Word# -> Word#
word32ToWordCompat# x = x

wordToWord8Compat# :: Word# -> Word#
wordToWord8Compat# x = x

wordToWord16Compat# :: Word# -> Word#
wordToWord16Compat# x = x
#endif
