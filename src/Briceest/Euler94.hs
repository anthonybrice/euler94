-- src/Briceest/Euler94.hs

--module Briceest.Euler94 (euler94) where

import           Data.List          (foldl')
import           System.Environment (getArgs)

isInteger :: Double -> Bool
isInteger x = x == (fromIntegral . round $ x)

area :: Integer -> Integer -> Double
area x y = y' * sqrt ((x' - y') * (x' + y'))
  where
    x' = fromIntegral x
    y' = fromIntegral y / 2

folder :: Integer -> Integer -> Integer
folder acc k =
    let (q, r) = quotRem k 3
    in case r of
        0 -> acc
        1 -> if isInteger $ area q (q + 1)
             then acc + k
             else acc
        2 -> if isInteger $ area (q + 1) q
             then acc + k
             else acc

euler94 :: (Integer, Integer) -- prior ending, prior sum
        -> Integer            -- new ending
        -> (Integer, Integer) -- new ending, new sum
euler94 (pend, psum) nend =
    (nend, foldl' folder psum [(pend + 1)..nend])
-- invoke as `euler94 (4, 0) n` to run from 5 to n

main :: IO ()
main = do
  (x':_) <- getArgs
  let x = read x' :: Integer
      y = euler94 (4, 0) x
  print y
