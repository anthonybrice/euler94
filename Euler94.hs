module Brice.Euler94 (euler94) where

import           Control.Monad (replicateM)
import           Data.List     (foldl', nub, sort, transpose)
import           Data.Map      (Map, fromList, (!))

-- | Returns true if the the given double is integer valued.
isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

-- | Returns the area of the triangle formed by the given side lengths. Does not
-- validate that the given lengths can actually form a triangle.
areaT :: Integer -> Integer -> Integer -> Double
areaT x y z = sqrt $ fromRational $ p * (p - x') * (p - y') * (p - z')
  where p  = (x' + y' + z') / 2 :: Rational
        x' = fromInteger x
        y' = fromInteger y
        z' = fromInteger z

-- | Returns true if the given triangle has integer-valued area.
intArea :: (Integer, Integer) -> Bool
intArea (x, y) = isInteger $ areaT x x y

-- | 'euler94' is the sum of the perimeters of all almost equilateral triangles
-- with integral side lengths and area and whose perimeters do not exceed the
-- argument.
euler94 :: Integer -> Integer
euler94 = euler94' 16

-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area between the triangles given by @m@ and @n@
-- (inclusive).
euler94' :: Integer -> Integer -> Integer
euler94' m n = sum . map (\(x, y) -> 2 * x + y) . filter intArea
               $ allTriangles' m n

-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides between the triangles given by @m@ and @n@. The first
-- element of the tuple gives the length of either similar side, and the second
-- element gives the length of the other side.
allTriangles' :: Integer -> Integer -> [(Integer, Integer)]
allTriangles' m n = takeWhile (\(p, q) -> 2 * p + q <= n) zs
  where (x, y)   = triangle m
        (x', y') = nextT (x, y)
        zs       = (concat . transpose) [ zip [x..n] [y..n]
                                        , zip [x'..n] [y'..n]
                                        ]

-- | Returns the almost equilateral triangle given by the perimeter @p@.
triangle :: Integer -> (Integer, Integer)
triangle p = if r == 1 then (q, q+1) else (q+1, q)
  where (q, r) = quotRem p 3

-- | Returns the next almost equilateral triangle after the given one.
nextT :: (Integer, Integer) -> (Integer, Integer)
nextT (x, y) = if x < y then (y, x) else (x, y+2)

-- | A binary operator for the fold in 'main'.
mainFold :: [(Integer, Integer)] -> Integer -> [(Integer, Integer)]
mainFold [] n = init $ mainFold [(15, 0)] n
mainFold acc n = (n, s + snd (head acc)) : acc
  where s = euler94' (fst (head acc) + 1) n

mapNs :: [Integer] -> Map Integer Integer
mapNs = fromList . foldl' mainFold [(15, 0)] . sort . nub

main :: IO ()
main = do
  ns <- readLn >>= flip replicateM readLn
  let vs = mapNs ns
  mapM_ (\n -> print $ vs!n) ns
