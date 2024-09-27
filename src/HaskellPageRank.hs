module HaskellPageRank where

import HaskellMathsFunctions (
  transpose,
  addMatrix,
  scalarMult,
  minusVector,
  dotMV
  )

initialRank :: Fractional a => [[a]] -> [a]
initialRank xs = [ 1 / (fromIntegral $ length xs)  | _ <- xs]

getP' :: Fractional a => [a] -> [a]
getP' xs = [1| _ <- xs]

getB :: Fractional a => [[a]] -> [[a]]
getB xs = [ [ 1 / (fromIntegral $ length xs) | _ <- x] | x <- xs ]

unSink :: (Fractional a, Eq a) => [[a]] -> [[a]]
unSink = transpose . f . transpose
    where f [] = []
          f m@(x:xs)
            | sum x == 0 = [[ 1 / (fromIntegral $ length $ head m) | _ <- x]] ++ f xs
            | otherwise = [x] ++ f xs

pageRank :: (Fractional a, Ord a) => [[a]] -> a -> a -> [a]
pageRank h d e = f (initialRank a) (getP' $ initialRank a)
    where
      a = unSink h
      b = getB a
      g = addMatrix (scalarMult d a) (scalarMult (1 - d) b)
      f p p'
        | sum (map abs (minusVector p' p)) > e = f (dotMV g p) p
        | otherwise = p