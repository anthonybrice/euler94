-- src/Briceest/Euler94.hs

module Briceest.Euler94 (euler94) where

import           Data.List (foldl')

isInteger :: Float -> Bool
isInteger x = x == (fromIntegral . round $ x)

area :: Integer -> Integer -> Float
area x y = y' * sqrt ((x' - y') * (x' + y'))
  where
    x' = fromIntegral x
    y' = fromIntegral y / 2

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

euler94 :: (Integer, Integer) -- prior ending, prior sum
        -> Integer            -- new ending
        -> (Integer, Integer) -- new ending, new sum
euler94 (pend, psum) nend =
    (nend, foldl' folder psum [(pend + 1)..nend])
-- invoke as `euler94 (4, 0) n` to run from 5 to n
