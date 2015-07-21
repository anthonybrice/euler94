import           Control.Monad (replicateM)

isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

areaT :: Double -> Double -> Double -> Double
areaT x y z = sqrt $ p * (p - x) * (p - y) * (p - z)
  where p = (x + y + z) / 2

allTriangles :: Integer -> [(Integer, Integer)]
allTriangles n = [(x, y) | x <- [2..(n-1)], y <- [1..(n-1)], abs (x - y) == 1]

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
