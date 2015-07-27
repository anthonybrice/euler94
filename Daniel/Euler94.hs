-- src/Briceest/Euler94.hs

-- my imports
import           Data.List (foldl')

-- arithmoi imports below
import Data.Array.Base (unsafeAt)

isSquare :: Integer -> Bool
isSquare = undefined

folder :: Integer -> Integer -> Integer
folder acc n =
    let (q, r) = quotRem n 3
    in case r of
        0 -> acc
        1 -> let (k, s) = quotRem q 2
             in case s of
                0 -> acc
                1 -> if isSquare $ k * (3 * k + 2)
                     then acc + n
                     else acc
        2 -> let (k, s) = quotRem q 2
             in case s of
                0 -> if isSquare $ (3 * k + 1) * (k + 1)
                     then acc + n
                     else acc
                1 -> acc

euler94 :: (Integer, Integer) -- prior last perimeter, prior sum
        -> Integer            -- new last perimeter
        -> (Integer, Integer) -- new last perimeter, new sum
euler94 (priorLast, priorSum) newLast =
    (newLast, foldl' folder priorSum [(priorLast + 1)..newLast])
-- invoke as `euler94 (4, 0) n` to run from 5 to n

------------------------------------------------------------------------
--  THERE BE DRAGONS HERE ----------------------------------------------

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
--{-# SPECIALISE isPossibleSquare2 :: Int -> Bool,
--                                    Integer -> Bool,
--                                    Word -> Bool
--  #-}
--isPossibleSquare2 :: Integral a => a -> Bool
--isPossibleSquare2 n =
--  unsafeAt sr256 ((fromIntegral n) .&. 255)
--  && unsafeAt sr819  (fromIntegral (n `rem` 819))
--  && unsafeAt sr1025 (fromIntegral (n `rem` 1025))
--  && unsafeAt sr2047 (fromIntegral (n `rem` 2047))
--  && unsafeAt sr4097 (fromIntegral (n `rem` 4097))
--  && unsafeAt sr341  (fromIntegral (n `rem` 341))