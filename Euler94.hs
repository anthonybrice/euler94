import           Control.Monad (replicateM)
import           Data.List     (transpose)

-- | Returns true if the the given double is integer valued.
isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

-- | Returns the area of the triangle formed by the given side lengths. Does not
-- validate that the given lengths can actually form a triangle.
areaT :: Integer -> Integer -> Integer -> Double
areaT x y z = sqrt $ fromRational $ p * (p - x') * (p - y') * (p - z')
  where p = (x' + y' + z') / 2 :: Rational
        x' = fromInteger x
        y' = fromInteger y
        z' = fromInteger z

-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides and perimeter less than or equal to @n@. The first
-- element of the tuple gives the length of either similar side, and the second
-- element gives the length of the other side.
allTriangles :: Integer -> [(Integer, Integer)]
allTriangles n = takeWhile (\(x, y) -> 2 * x + y <= n) zs
  where zs = (concat . transpose) [ zip [5..n] [6..n]
                                  , zip [6..n] [5..n]
                                  ]
--
-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area whose perimeters are less than or equal to
-- @n@.
euler94 :: Integer -> Integer
euler94 n = sum $ map (\(x, y) -> 2 * x + y) $ filter intArea $ allTriangles n

perimeters :: Integer -> [Integer]
perimeters n = filter (\x -> x `rem` 3 /= 0) [16..n]

intArea :: (Integer, Integer) -> Bool
intArea (x, y) = isInteger $ areaT x x y

intArea' :: Integer -> Bool
intArea' p =
  let (q, r) = quotRem p 3
      (x, y) = if r == 1
               then (q, q+1)
               else (q+1, q)
      in isInteger $ areaT x x y

main :: IO ()
main = readLn >>= flip replicateM readLn >>= mapM_ (print . euler94)
