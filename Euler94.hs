import           Control.Monad (replicateM)
import           Data.List     (foldl')
import           Data.Tuple    (swap)

infixl 9 #
(#) :: (a -> b) -> (b -> c) -> a -> c
-- ^ convenience infix op for reverse function composition
(#) = flip (.)


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

--duplicateEach = (>>= (replicate 2 >>= (\[x] -> [head x, swap $ last x])))

-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides and perimeter less than or equal to @n@. The first
-- element of the tuple is the length of two sides, and the second element is
-- the length of the other side.
allTriangles :: Integer -> [(Integer, Integer)]
allTriangles n = do
  let zs = zip [2..n] [1..n]
      zs' = head zs : do z <- tail zs
                         [swap z, z]
  takeWhile (\(x, y) -> 2 * x + y <= n) zs'

-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area whose perimeters are less than or equal to
-- @n@.
euler94 :: Integer -> Integer
euler94 n = foldr (\(x, y) acc -> if isInteger $ areaT x x y
                                  then acc + 2 * x + y
                                  else acc) 0 $ allTriangles n

main :: IO ()
main = do
  ns <- readLn >>= flip replicateM readLn :: IO [Integer]
  mapM_ (print . euler94) ns
