-- Math.NumberTheory.Utils
{-# LANGUAGE CPP, MagicHash, UnboxedTuples, BangPatterns, FlexibleContexts #-}

#include "MachDeps.h"

import GHC.Base
#if __GLASGOW_HASKELL__ < 705
import GHC.Word     -- Word and its constructor moved to GHC.Types
#endif

import GHC.Integer
import GHC.Integer.GMP.Internals

import Data.Bits

import Data.Array.Unboxed
import Data.Array.ST
import Data.Array.Base (unsafeAt, unsafeWrite)

#if WORD_SIZE_IN_BITS == 64
#define m5 0x5555555555555555
#define m3 0x3333333333333333
#define mf 0x0F0F0F0F0F0F0F0F
#define m1 0x0101010101010101
#define sd 56
#define WSHIFT 6
#define MMASK 63
#else
#define m5 0x55555555
#define m3 0x33333333
#define mf 0x0F0F0F0F
#define m1 0x01010101
#define sd 24
#define WSHIFT 5
#define MMASK 31
#endif

uncheckedShiftR :: Word -> Int -> Word
uncheckedShiftR (W# w#) (I# i#) = W# (uncheckedShiftRL# w# i#)

-- | Remove factors of @2@ and count them. If
--   @n = 2^k*m@ with @m@ odd, the result is @(k, m)@.
--   Precondition: argument not @0@ (not checked).
{-# RULES
"shiftToOddCount/Int"       shiftToOddCount = shiftOCInt
"shiftToOddCount/Word"      shiftToOddCount = shiftOCWord
"shiftToOddCount/Integer"   shiftToOddCount = shiftOCInteger
  #-}
{-# INLINE [1] shiftToOddCount #-}
shiftToOddCount :: (Integral a, Bits a) => a -> (Int, a)
shiftToOddCount n = case shiftOCInteger (fromIntegral n) of
                      (z, o) -> (z, fromInteger o)

-- | Specialised version for @'Word'@.
--   Precondition: argument strictly positive (not checked).
shiftOCWord :: Word -> (Int, Word)
shiftOCWord (W# w#) = case shiftToOddCount# w# of
                        (# z# , u# #) -> (I# z#, W# u#)

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOCInt :: Int -> (Int, Int)
shiftOCInt (I# i#) = case shiftToOddCount# (int2Word# i#) of
                        (# z#, u# #) -> (I# z#, I# (word2Int# u#))

-- | Specialised version for @'Integer'@.
--   Precondition: argument nonzero (not checked).
shiftOCInteger :: Integer -> (Int, Integer)
shiftOCInteger n@(S# i#) =
    case shiftToOddCount# (int2Word# i#) of
      (# z#, w# #)
        | isTrue# (z# ==# 0#) -> (0, n)
        | otherwise -> (I# z#, S# (word2Int# w#))
#if __GLASGOW_HASKELL__ < 709
shiftOCInteger n@(J# _ ba#) = case count 0# 0# of
#else
shiftOCInteger n@(Jp# bn#) = case bigNatZeroCount bn# of
                                 0#  -> (0, n)
                                 z#  -> (I# z#, n `shiftRInteger` z#)
shiftOCInteger n@(Jn# bn#) = case bigNatZeroCount bn# of
#endif
                                 0#  -> (0, n)
                                 z#  -> (I# z#, n `shiftRInteger` z#)
#if __GLASGOW_HASKELL__ < 709
  where
    count a# i# =
          case indexWordArray# ba# i# of
             0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
             w#  -> a# +# trailZeros# w#
#endif

#if __GLASGOW_HASKELL__ >= 709
-- | Count trailing zeros in a @'BigNat'@.
--   Precondition: argument nonzero (not checked, Integer invariant).
bigNatZeroCount :: BigNat -> Int#
bigNatZeroCount bn# = count 0# 0#
  where
    count a# i# =
          case indexBigNat# bn# i# of
            0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
            w#  -> a# +# trailZeros# w#
#endif

-- | Remove factors of @2@. If @n = 2^k*m@ with @m@ odd, the result is @m@.
--   Precondition: argument not @0@ (not checked).
{-# RULES
"shiftToOdd/Int"       shiftToOdd = shiftOInt
"shiftToOdd/Word"      shiftToOdd = shiftOWord
"shiftToOdd/Integer"   shiftToOdd = shiftOInteger
  #-}
{-# INLINE [1] shiftToOdd #-}
shiftToOdd :: (Integral a, Bits a) => a -> a
shiftToOdd n = fromInteger (shiftOInteger (fromIntegral n))

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOInt :: Int -> Int
shiftOInt (I# i#) = I# (word2Int# (shiftToOdd# (int2Word# i#)))

-- | Specialised version for @'Word'@.
--   Precondition: argument nonzero (not checked).
shiftOWord :: Word -> Word
shiftOWord (W# w#) = W# (shiftToOdd# w#)

-- | Specialised version for @'Int'@.
--   Precondition: argument nonzero (not checked).
shiftOInteger :: Integer -> Integer
shiftOInteger (S# i#) = S# (word2Int# (shiftToOdd# (int2Word# i#)))
#if __GLASGOW_HASKELL__ < 709
shiftOInteger n@(J# _ ba#) = case count 0# 0# of
#else
shiftOInteger n@(Jn# bn#) = case bigNatZeroCount bn# of
                                 0#  -> n
                                 z#  -> n `shiftRInteger` z#
shiftOInteger n@(Jp# bn#) = case bigNatZeroCount bn# of
#endif
                                 0#  -> n
                                 z#  -> n `shiftRInteger` z#
#if __GLASGOW_HASKELL__ < 709
  where
    count a# i# =
          case indexWordArray# ba# i# of
            0## -> count (a# +# WORD_SIZE_IN_BITS#) (i# +# 1#)
            w#  -> a# +# trailZeros# w#
#endif

-- | Shift argument right until the result is odd.
--   Precondition: argument not @0@, not checked.
shiftToOdd# :: Word# -> Word#
shiftToOdd# w# = case trailZeros# w# of
                   k# -> uncheckedShiftRL# w# k#

-- | Like @'shiftToOdd#'@, but count the number of places to shift too.
shiftToOddCount# :: Word# -> (# Int#, Word# #)
shiftToOddCount# w# = case trailZeros# w# of
                        k# -> (# k#, uncheckedShiftRL# w# k# #)

-- | Number of 1-bits in a @'Word#'@.
bitCountWord# :: Word# -> Int#
bitCountWord# w# = case bitCountWord (W# w#) of
                     I# i# -> i#

-- | Number of 1-bits in a @'Word'@.
bitCountWord :: Word -> Int
#if __GLASGOW_HASKELL__ >= 703
bitCountWord = popCount
-- should yield a machine instruction
#else
bitCountWord w = case w - (shiftR w 1 .&. m5) of
                   !w1 -> case (w1 .&. m3) + (shiftR w1 2 .&. m3) of
                            !w2 -> case (w2 + shiftR w2 4) .&. mf of
                                     !w3 -> fromIntegral (shiftR (w3 * m1) sd)
#endif

-- | Number of 1-bits in an @'Int'@.
bitCountInt :: Int -> Int
#if __GLASGOW_HASKELL__ >= 703
bitCountInt = popCount
-- should yield a machine instruction
#else
bitCountInt (I# i#) = bitCountWord (W# (int2Word# i#))
#endif

-- | Number of trailing zeros in a @'Word#'@, wrong for @0@.
{-# INLINE trailZeros# #-}
trailZeros# :: Word# -> Int#
trailZeros# w =
    case xor# w (w `minusWord#` 1##) `uncheckedShiftRL#` 1# of
      v0 ->
        case v0 `minusWord#` (uncheckedShiftRL# v0 1# `and#` m5##) of
          v1 ->
            case (v1 `and#` m3##) `plusWord#` (uncheckedShiftRL# v1 2# `and#` m3##) of
              v2 ->
                case (v2 `plusWord#` uncheckedShiftRL# v2 4#) `and#` mf## of
                  v3 -> word2Int# (uncheckedShiftRL# (v3 `timesWord#` m1##) sd#)

-- {-# SPECIALISE splitOff :: Integer -> Integer -> (Int, Integer),
--                            Int -> Int -> (Int, Int),
--                            Word -> Word -> (Int, Word)
--   #-}
#if __GLASGOW_HASKELL__ >= 700
{-# INLINABLE splitOff #-}
#else
{-# INLINE splitOff #-}
#endif
splitOff :: Integral a => a -> a -> (Int, a)
splitOff p n = go 0 n
  where
    go !k m = case m `quotRem` p of
                (q,r) | r == 0 -> go (k+1) q
                      | otherwise -> (k,m)

#if __GLASGOW_HASKELL__ < 707
-- The times they are a-changing. The types of primops too :(
isTrue# :: Bool -> Bool
isTrue# = id
#endif


-- Math.NumberTheory.Logarithms.Internal

-- | Calculate the integer base 2 logarithm of an 'Integer'.
--   The calculation is much more efficient than for the general case.
--
--   The argument must be strictly positive, that condition is /not/ checked.
integerLog2# :: Integer -> Int#
integerLog2# (S# i) = wordLog2# (int2Word# i)
integerLog2# (J# s ba) = check (s -# 1#)
  where
    check i = case indexWordArray# ba i of
                0## -> check (i -# 1#)
                w   -> wordLog2# w +# (uncheckedIShiftL# i WSHIFT#)

-- | This function calculates the integer base 2 logarithm of a 'Word#'.
--   @'wordLog2#' 0## = -1#@.
{-# INLINE wordLog2# #-}
wordLog2# :: Word# -> Int#
wordLog2# w =
  case leadingZeros of
   BA lz ->
    let zeros u = indexInt8Array# lz (word2Int# u) in
#if WORD_SIZE_IN_BITS == 64
    case uncheckedShiftRL# w 56# of
     a ->
      if a `neWord#` 0##
       then 64# -# zeros a
       else
        case uncheckedShiftRL# w 48# of
         b ->
          if b `neWord#` 0##
           then 56# -# zeros b
           else
            case uncheckedShiftRL# w 40# of
             c ->
              if c `neWord#` 0##
               then 48# -# zeros c
               else
                case uncheckedShiftRL# w 32# of
                 d ->
                  if d `neWord#` 0##
                   then 40# -# zeros d
                   else
#endif
                    case uncheckedShiftRL# w 24# of
                     e ->
                      if e `neWord#` 0##
                       then 32# -# zeros e
                       else
                        case uncheckedShiftRL# w 16# of
                         f ->
                          if f `neWord#` 0##
                           then 24# -# zeros f
                           else
                            case uncheckedShiftRL# w 8# of
                             g ->
                              if g `neWord#` 0##
                               then 16# -# zeros g
                               else 8# -# zeros w

-- Lookup table
data BA = BA ByteArray#

leadingZeros :: BA
leadingZeros =
    let mkArr s =
          case newByteArray# 256# s of
            (# s1, mba #) ->
              case writeInt8Array# mba 0# 9# s1 of
                s2 ->
                  let fillA lim val idx st =
                        if idx ==# 256#
                          then st
                          else if idx <# lim
                                then case writeInt8Array# mba idx val st of
                                        nx -> fillA lim val (idx +# 1#) nx
                                else fillA (2# *# lim) (val -# 1#) idx st
                  in case fillA 2# 8# 1# s2 of
                      s3 -> case unsafeFreezeByteArray# mba s3 of
                              (# _, ba #) -> ba
    in case mkArr realWorld# of
        b -> BA b

#endif


-- Math.NumberTheory.Powers.Squares

-- | Calculate the integer square root of a nonnegative number @n@,
--   that is, the largest integer @r@ with @r*r <= n@.
--   Throws an error on negative input.
{-# SPECIALISE integerSquareRoot :: Int -> Int,
                                    Word -> Word,
                                    Integer -> Integer
  #-}
integerSquareRoot :: Integral a => a -> a
integerSquareRoot n
  | n < 0       = error "integerSquareRoot: negative argument"
  | otherwise   = integerSquareRoot' n

-- | Calculate the integer square root of a nonnegative number @n@,
--   that is, the largest integer @r@ with @r*r <= n@.
--   The precondition @n >= 0@ is not checked.
{-# RULES
"integerSquareRoot'/Int"  integerSquareRoot' = isqrtInt'
"integerSquareRoot'/Word" integerSquareRoot' = isqrtWord
  #-}
{-# INLINE [1] integerSquareRoot' #-}
integerSquareRoot' :: Integral a => a -> a
integerSquareRoot' = isqrtA

-- | Returns 'Nothing' if the argument is not a square,
--   @'Just' r@ if @r*r == n@ and @r >= 0@. Avoids the expensive calculation
--   of the square root if @n@ is recognized as a non-square
--   before, prevents repeated calculation of the square root
--   if only the roots of perfect squares are needed.
--   Checks for negativity and 'isPossibleSquare'.
{-# SPECIALISE exactSquareRoot :: Int -> Maybe Int,
                                  Word -> Maybe Word,
                                  Integer -> Maybe Integer
  #-}
exactSquareRoot :: Integral a => a -> Maybe a
exactSquareRoot n
  | n < 0                           = Nothing
  | isPossibleSquare n && r*r == n  = Just r
  | otherwise                       = Nothing
    where
      r = integerSquareRoot' n

-- | Test whether the argument is a square.
--   After a number is found to be positive, first 'isPossibleSquare'
--   is checked, if it is, the integer square root is calculated.
{-# SPECIALISE isSquare :: Int -> Bool,
                           Word -> Bool,
                           Integer -> Bool
  #-}
isSquare :: Integral a => a -> Bool
isSquare n = n >= 0 && isSquare' n

-- | Test whether the input (a nonnegative number) @n@ is a square.
--   The same as 'isSquare', but without the negativity test.
--   Faster if many known positive numbers are tested.
--
--   The precondition @n >= 0@ is not tested, passing negative
--   arguments may cause any kind of havoc.
{-# SPECIALISE isSquare' :: Int -> Bool,
                            Word -> Bool,
                            Integer -> Bool
  #-}
isSquare' :: Integral a => a -> Bool
isSquare' n = isPossibleSquare n && let r = integerSquareRoot' n in r*r == n

-- | Test whether a non-negative number may be a square.
--   Non-negativity is not checked, passing negative arguments may
--   cause any kind of havoc.
--
--   First the remainder modulo 256 is checked (that can be calculated
--   easily without division and eliminates about 82% of all numbers).
--   After that, the remainders modulo 9, 25, 7, 11 and 13 are tested
--   to eliminate altogether about 99.436% of all numbers.
--
--   This is the test used by 'exactSquareRoot'. For large numbers,
--   the slower but more discriminating test 'isPossibleSqure2' is
--   faster.
{-# SPECIALISE isPossibleSquare :: Int -> Bool,
                                   Integer -> Bool,
                                   Word -> Bool
  #-}
isPossibleSquare :: Integral a => a -> Bool
isPossibleSquare n =
  unsafeAt sr256 ((fromIntegral n) .&. 255)
  && unsafeAt sr693 (fromIntegral (n `rem` 693))
  && unsafeAt sr325 (fromIntegral (n `rem` 325))

-- | Test whether a non-negative number may be a square.
--   Non-negativity is not checked, passing negative arguments may
--   cause any kind of havoc.
--
--   First the remainder modulo 256 is checked (that can be calculated
--   easily without division and eliminates about 82% of all numbers).
--   After that, the remainders modulo several small primes are tested
--   to eliminate altogether about 99.98954% of all numbers.
--
--   For smallish to medium sized numbers, this hardly performs better
--   than 'isPossibleSquare', which uses smaller arrays, but for large
--   numbers, where calculating the square root becomes more expensive,
--   it is much faster (if the vast majority of tested numbers aren't squares).
{-# SPECIALISE isPossibleSquare2 :: Int -> Bool,
                                    Integer -> Bool,
                                    Word -> Bool
  #-}
isPossibleSquare2 :: Integral a => a -> Bool
isPossibleSquare2 n =
  unsafeAt sr256 ((fromIntegral n) .&. 255)
  && unsafeAt sr819  (fromIntegral (n `rem` 819))
  && unsafeAt sr1025 (fromIntegral (n `rem` 1025))
  && unsafeAt sr2047 (fromIntegral (n `rem` 2047))
  && unsafeAt sr4097 (fromIntegral (n `rem` 4097))
  && unsafeAt sr341  (fromIntegral (n `rem` 341))

-----------------------------------------------------------------------------
--  Auxiliary Stuff

-- Find approximation to square root in 'Integer', then
-- find the integer square root by the integer variant
-- of Heron's method. Takes only a handful of steps
-- unless the input is really large.
{-# SPECIALISE isqrtA :: Integer -> Integer #-}
isqrtA :: Integral a => a -> a
isqrtA 0 = 0
isqrtA n = heron n (fromInteger . appSqrt . fromIntegral $ n)

-- Heron's method for integers. First make one step to ensure
-- the value we're working on is @>= r@, then we have
-- @k == r@ iff @k <= step k@.
{-# SPECIALISE heron :: Integer -> Integer -> Integer #-}
heron :: Integral a => a -> a -> a
heron n a = go (step a)
      where
        step k = (k + n `quot` k) `quot` 2
        go k
            | m < k     = go m
            | otherwise = k
              where
                m = step k

-- threshold for shifting vs. direct fromInteger
-- we shift when we expect more than 256 bits
#if WORD_SIZE_IN_BITS == 64
#define THRESH 5
#else
#define THRESH 9
#endif

-- Find a fairly good approximation to the square root.
-- At most one off for small Integers, about 48 bits should be correct
-- for large Integers.
appSqrt :: Integer -> Integer
appSqrt (S# i#) = S# (double2Int# (sqrtDouble# (int2Double# i#)))
#if __GLASGOW_HASKELL__ < 709
appSqrt n@(J# s# _)
    | isTrue# (s# <# THRESH#) = floor (sqrt $ fromInteger n :: Double)
#else
appSqrt n@(Jp# bn#)
    | isTrue# ((sizeofBigNat# bn#) <# THRESH#) =
          floor (sqrt $ fromInteger n :: Double)
#endif
    | otherwise = case integerLog2# n of
                    l# -> case uncheckedIShiftRA# l# 1# -# 47# of
                            h# -> case shiftRInteger n (2# *# h#) of
                                    m -> case floor (sqrt $ fromInteger m :: Double) of
                                            r -> shiftLInteger r h#
#if __GLASGOW_HASKELL__ >= 709
-- There's already a check for negative in integerSquareRoot,
-- but integerSquareRoot' is exported directly too.
appSqrt _ = error "integerSquareRoot': negative argument"
#endif

-- Auxiliaries

-- Make an array indicating whether a remainder is a square remainder.
sqRemArray :: Int -> UArray Int Bool
sqRemArray md = runSTUArray $ do
  arr <- newArray (0,md-1) False
  let !stop = (md `quot` 2) + 1
      fill k
        | k < stop  = unsafeWrite arr ((k*k) `rem` md) True >> fill (k+1)
        | otherwise = return arr
  unsafeWrite arr 0 True
  unsafeWrite arr 1 True
  fill 2

sr256 :: UArray Int Bool
sr256 = sqRemArray 256

sr819 :: UArray Int Bool
sr819 = sqRemArray 819

sr4097 :: UArray Int Bool
sr4097 = sqRemArray 4097

sr341 :: UArray Int Bool
sr341 = sqRemArray 341

sr1025 :: UArray Int Bool
sr1025 = sqRemArray 1025

sr2047 :: UArray Int Bool
sr2047 = sqRemArray 2047

sr693 :: UArray Int Bool
sr693 = sqRemArray 693

sr325 :: UArray Int Bool
sr325 = sqRemArray 325

-- Specialisations for Int and Word

-- For @n <= 2^64@, the result of
--
-- > truncate (sqrt $ fromIntegral n)
--
-- is never too small and never more than one too large.
-- The multiplication doesn't overflow for 32 or 64 bit Ints.
isqrtInt' :: Int -> Int
isqrtInt' n
    | n < r*r   = r-1
    | otherwise = r
      where
        !r = (truncate :: Double -> Int) . sqrt $ fromIntegral n
-- With -O2, that should be translated to the below
{-
isqrtInt' n@(I# i#)
    | r# *# r# ># i#            = I# (r# -# 1#)
    | otherwise                 = I# r#
      where
        !r# = double2Int# (sqrtDouble# (int2Double# i#))
-}

-- Same for Word.
isqrtWord :: Word -> Word
isqrtWord n
    | n < (r*r)
#if WORD_SIZE_IN_BITS == 64
      || r == 4294967296
-- Double interprets values near maxBound as 2^64, we don't have that problem for 32 bits
#endif
                = r-1
    | otherwise = r
      where
        !r = (fromIntegral :: Int -> Word) . (truncate :: Double -> Int) . sqrt $ fromIntegral n
z