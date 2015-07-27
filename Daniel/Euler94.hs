isSquare' :: Integer -> Bool
isSquare' = undefined

folder :: Integer -> Integer -> Integer
folder acc n =
    let (q, r) = quotRem n 3
    in case r of
        0 -> acc
        1 -> let (k, s) = quotRem q 2
             in case s of
                0 -> acc
                1 -> if isSquare' $ k * (3 * k + 2)
                     then acc + n
                     else acc
        2 -> let (k, s) = quotRem q 2
             in case s of
                0 -> if isSquare' $ (3 * k + 1) * (k + 1)
                     then acc + n
                     else acc
                1 -> acc

euler94 :: (Integer, Integer) -- prior last perimeter, prior sum
        -> Integer            -- new last perimeter
        -> (Integer, Integer) -- new last perimeter, new sum
euler94 (priorLast, priorSum) newLast =
    (newLast, foldl' folder priorSum [(priorLast + 1)..newLast])
-- invoke as `euler94 (4, 0) n` to run from 5 to n
