import Data.List (foldl')

isInteger :: Float -> Bool
isInteger x = x == (fromIntegral . round $ x)

area :: Integer -> Integer -> Float
area x y = y' * sqrt ((x' - y') * (x' + y'))
  where
    x' = fromIntegral x
    y' = fromIntegral y / 2

folder :: Integer -> Integer -> Integer
folder acc k =
    let (q, r) = quotRem k 3
    in case r of
        0 -> acc
        1 -> if isInteger $ area q (q + 1)
             then acc + k
             else acc
        2 -> if isInteger $ area (q + 1) q
             then acc + k
             else acc

euler94 :: (Integer, Integer) -- prior ending, prior sum
        -> Integer            -- new ending
        -> (Integer, Integer) -- new ending, new sum
euler94 (pend, psum) nend =
    (nend, foldl' folder psum [(pend + 1)..nend])
-- invoke as `euler94 (4, 0) n` to run from 5 to n
