module Brice.Euler94.Bin.Main (main) where

import           Brice.Euler94
import           Control.Monad (replicateM)
import           Data.List     (foldl', nub, sort)
import           Data.Map      (Map, fromList, (!))

mainFold :: Integral a => [(a, a)] -> a -> [(a, a)]
mainFold acc n = (n, s + snd (head acc)) : acc
  where s = euler94' (fst (head acc) + 1) n

-- | 'mapNs' is a map from the given list to their 'euler94' value.
mapNs :: Integral a => [a] -> Map a a
mapNs = fromList . init . foldl' mainFold [(15, 0)] . sort . nub

main :: IO ()
main = do
  ns <- (readLn >>= flip replicateM readLn) :: IO [Integer]
  let vs = mapNs ns
  mapM_ (\n -> print $ vs!n) ns
