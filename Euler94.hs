import           Control.Monad (replicateM)
import           Data.List     (foldl')
import           Data.Tuple    (swap)

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

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x:xs) ys = x : merge ys xs

-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides and perimeter less than or equal to @n@. The first
-- element of the tuple gives the length of either similar side, and the second
-- element gives the length of the other side.
allTriangles :: Integer -> [(Integer, Integer)]
allTriangles n = takeWhile (\(x, y) -> 2 * x + y <= n) zs
  where zs = (2, 1) : merge (zip [2..n] [3..n]) (zip [3..n] [2..n])

-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area whose perimeters are less than or equal to
-- @n@.
euler94 :: Integer -> Integer
euler94 n = foldl' (\acc (x, y) -> if isInteger $ areaT x x y
                                   then acc + 2 * x + y
                                   else acc) 0 $ allTriangles n

main :: IO ()
main = do
  ns <- readLn >>= flip replicateM readLn :: IO [Integer]
  mapM_ (print . euler94) ns
