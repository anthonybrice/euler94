module Brice.Euler94 (euler94, euler94') where

import           Data.List                        (transpose)
import           Math.NumberTheory.Powers.Squares (isSquare)

-- | 'euler94' is the sum of the perimeters of all almost equilateral triangles
-- with integral side lengths and area and whose perimeters do not exceed the
-- argument.
euler94 :: Integral a => a -> a
euler94 = euler94' 16

-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area between the triangles given by @m@ and @n@
-- (inclusive).
euler94' :: Integral a => a -> a -> a
euler94' m n = sum . map (\(x, y) -> 2 * x + y) . filter intArea
               $ allTriangles m n

-- | Returns true if the given triangle has integer-valued area.
intArea :: Integral a => (a, a) -> Bool
intArea (x, y)
  | odd y     = False
  | otherwise = isSquare $ s * (s - x) * (s - x) * (s - y)
  where s = (2 * x + y) `quot` 2

-- | Returns true if the the given double is integer valued.
isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

-- | Returns the area of the triangle formed by the given side lengths. Does not
-- validate that the given lengths can actually form a triangle.
area :: Integral a => a -> a -> Double
area x y = y' * sqrt ((x' - y') * (x' + y'))
  where x' = fromIntegral x
        y' = fromIntegral y / 2

-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides between the triangles given by @m@ and @n@. The first
-- element of the tuple gives the length of either similar side, and the second
-- element gives the length of the other side.
allTriangles :: Integral a => a -> a -> [(a, a)]
allTriangles m n = takeWhile (\(p, q) -> 2 * p + q <= n) zs
  where (x, y)   = triangle m
        (x', y') = nextT (x, y)
        zs       = concat . transpose $ [ zip [x..n] [y..n]
                                        , zip [x'..n] [y'..n]
                                        ]

-- | Returns the almost equilateral triangle given by the perimeter @p@.
triangle :: Integral a => a -> (a, a)
triangle p = if r == 1 then (q, q+1) else (q+1, q)
  where (q, r) = quotRem p 3

-- | Returns the next almost equilateral triangle after the given one.
nextT :: Integral a => (a, a) -> (a, a)
nextT (x, y) = if x < y then (y, x) else (x, y+2)
