import           Control.Monad (replicateM)
import           Data.List     (elemIndex, foldl', nub, sort, transpose)
import           Data.Maybe    (fromJust)

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

-- | Returns a list of tuples of all almost equilateral triangles with
-- integer-valued sides and perimeter less than or equal to @n@. The first
-- element of the tuple gives the length of either similar side, and the second
-- element gives the length of the other side.
allTriangles :: Integer -> [(Integer, Integer)]
allTriangles n = takeWhile (\(x, y) -> 2 * x + y <= n) zs
  where zs = (2, 1) : (concat . transpose) [ zip [2..n] [3..n]
                                           , zip [3..n] [2..n]
                                           ]

-- | Returns the sum of the perimeters of all almost equilateral triangles with
-- integral side lengths and area whose perimeters are less than or equal to
-- @n@.
euler94 :: Integer -> Integer
euler94 = allTriangles
          # flip foldl' 0 (\acc (x, y) -> if isInteger $ areaT x x y
                                          then acc + 2 * x + y
                                          else acc)

euler94' :: (Integer, (Integer, Integer))
         -> Integer
         -> (Integer, (Integer, Integer))
euler94' (p, t) n =
  let (ts, nt) = allTriangles' t n
      s = flip foldl' p (\acc (x, y) -> if isInteger $ areaT x x y
                                        then acc + 2 * x + y
                                        else acc) ts
  in (s, nt)


allTriangles' :: (Integer, Integer)
              -> Integer
              -> ([(Integer, Integer)], (Integer, Integer))
allTriangles' t@(p, q) n =
  let ts = takeWhile (\(x, y) -> 2 * x + y <= n) zs
      (p', q') = nextT t
      zs = (concat . transpose) [ zip [p..n] [q..n]
                                , zip [p'..n] [q'..n]
                                ]
      nextT (y, z) = if y < z
                     then (z, y)
                     else (y, z+2)
      t' = nextT $ last ts
  in ts `seq` t' `seq` (ts, t')

mainFold acc n =
  let (s, nt) = euler94' (head acc) n
  in s `seq` nt `seq` (s, nt) : acc

main :: IO ()
main = do
  ns <- readLn >>= flip replicateM readLn
  let ns' = sort . nub $ ns
      vs = reverse . init $! foldl' mainFold [(0, (2,1))] ns'
      _ = map (`seq` undefined) vs
  mapM_ (\n -> let i = fromJust $! elemIndex n ns'
               in print $! fst $! vs!!i) ns
