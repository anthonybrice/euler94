-- src/Briceest/Euler94.hs

--module Briceest.Euler94 (euler94) where

import           Data.List (foldl', nub, sort)
import Control.Monad (replicateM)
import Data.Map (Map, fromList, (!))

isInteger :: Double -> Bool
isInteger x = x == (fromIntegral . round $ x)

area :: Integral a => a -> a -> Double
area x y = y' * sqrt ((x' - y') * (x' + y'))
  where
    x' = fromIntegral x
    y' = fromIntegral y / 2

folder :: Integral a => a -> a -> a
folder acc n =
    let (q, r) = quotRem n 3
    in case r of
        0 -> acc
        1 -> let (k, s) = quotRem q 2
             in case s of
                0 -> acc
                1 -> if isInteger . sqrt . fromIntegral $ k * (3 * k + 2)
                     then acc + n
                     else acc
        2 -> let (k, s) = quotRem q 2
             in case s of
                0 -> if isInteger . sqrt . fromIntegral $ (3 * k + 1) * (k + 1)
                     then acc + n
                     else acc
                1 -> acc

euler94 :: Integral a => (a, a) -- prior ending, prior sum
        -> a         -- new ending
        -> (a, a) -- new ending, new sum
euler94 (pend, psum) nend =
    (nend, foldl' folder psum [(pend + 1)..nend])
-- invoke as `euler94 (4, 0) n` to run from 5 to n

mainFold :: Integral a => [(a, a)] -> a -> [(a, a)]
mainFold acc@(x:_) n = s : acc
  where s = euler94 x n

mapNs :: Integral a => [a] -> Map a a
mapNs = fromList . init . foldl' mainFold [(4,0)] . sort . nub

main = do
  ns <- (readLn >>= flip replicateM readLn) :: IO [Integer]
  let vs = mapNs ns
  mapM_ (\n -> print $ vs!n) ns
