import           Control.Monad (replicateM)

-- | Returns true if the the given double is integer valued.
isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

-- | Returns the area of the triangle formed by the given side lengths. Does not
-- validate that the given lengths can actually form a triangle.
areaT :: Double -> Double -> Double -> Double
areaT x y z = sqrt $ p * (p - x) * (p - y) * (p - z)
  where p = (x + y + z) / 2

-- | Returns a list of tuples of all almost equilateral triangles
-- whose perimeters are less than @n@. The first element of the tuple is the
-- length of the shorter sides, and the second element is the length of the
-- longer side.
allTriangles :: Integer -> [(Integer, Integer)]
allTriangles n = [(x, y) | x <- [2..n]
                         , y <- [1..n]
                         , abs (x - y) == 1
                         , 2 * x + y < n
                         ]

-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area whose perimeters are less than @n@.
euler94 :: Integer -> Integer
euler94 n = sum $ do
  (x, y) <- allTriangles n
  if isInteger $ areaT (fromInteger x) (fromInteger x) (fromInteger y)
    then return $ sum [x, x, y]
    else fail "not an integer-valued area"

main :: IO ()
main = do
  ns <- readLn >>= flip replicateM readLn :: IO [Integer]
  mapM_ (print . euler94) ns
