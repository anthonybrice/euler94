--module Brice.Euler94 (euler94, euler94') where

import           Control.Monad      (replicateM)
import           Data.List          (foldl', nub, sort, transpose)
import           Data.Map           (Map, fromList, (!))
import           System.Environment (getArgs)

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
               $ allTriangles' m n

-- | Returns true if the given triangle has integer-valued area.
intArea :: Integral a => (a, a) -> Bool
intArea (x, y) = isInteger $ area x y

-- | Returns true if the the given double is integer valued.
isInteger :: Double-> Bool
isInteger x = x == fromInteger (round x)

-- | Returns the area of the triangle formed by the given side lengths. Does not
-- validate that the given lengths can actually form a triangle.
areaT :: Integral a => a -> a -> a -> Double
areaT x y z = sqrt $ fromRational $ p * (p - x') * (p - y') * (p - z')
  where p  = (x' + y' + z') / 2 :: Rational
        x' = fromIntegral x
        y' = fromIntegral y
        z' = fromIntegral z

area :: Integral a => a -> a -> Double
area x y = y' * sqrt ((x' - y') * (x' + y'))
  where
    x' = fromIntegral x
    y' = fromIntegral y / 2


-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides between the triangles given by @m@ and @n@. The first
-- element of the tuple gives the length of either similar side, and the second
-- element gives the length of the other side.
allTriangles' :: Integral a => a -> a -> [(a, a)]
allTriangles' m n = takeWhile (\(p, q) -> 2 * p + q <= n) zs
  where (x, y)   = triangle m
        (x', y') = nextT (x, y)
        zs       = (concat . transpose) [ zip [x..n] [y..n]
                                        , zip [x'..n] [y'..n]
                                        ]

-- | Returns the almost equilateral triangle given by the perimeter @p@.
triangle :: Integral a => a -> (a, a)
triangle p = if r == 1 then (q, q+1) else (q+1, q)
  where (q, r) = quotRem p 3

-- | Returns the next almost equilateral triangle after the given one.
nextT :: Integral a => (a, a) -> (a, a)
nextT (x, y) = if x < y then (y, x) else (x, y+2)

-- mainFold :: Integral a => [(a, a)] -> a -> [(a, a)]
-- mainFold acc n = (n, s + snd (head acc)) : acc
--   where s = euler94' (fst (head acc) + 1) n

-- -- | 'mapNs' is a map from the given list to their 'euler94' value.
-- mapNs :: Integral a => [a] -> Map a a
-- mapNs = fromList . init . foldl' mainFold [(15, 0)] . sort . nub

-- main :: IO ()
-- main = do
--   ns <- (readLn >>= flip replicateM readLn) :: IO [Integer]
--   let vs = mapNs ns
--   mapM_ (\n -> print $ vs!n) ns

main :: IO ()
main = do
  x':_ <- getArgs
  let x = read x'
      y = euler94 x
  print y
