import Data.Char
import Data.List

fact2 :: (Eq a, Num a) => a -> a -> a
fact2 0 n = n
fact2 x n = fact2 (x-1) (x*n)






























length' :: [a] -> Int
length' xs = len xs 0
  where len []     n = n
        len (x:xs) n = len xs (n+1)
        
length2' :: [a] -> Int
length2' xs = len xs 0
  where len []     n = n
        len (x:xs) n = let a = n+1 in a `seq` len xs a








maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip (x:xs) = maxUnzip' xs x
  where maxUnzip' [] pair = pair
        maxUnzip' ((a1, b1):ys) (a2, b2) = maxUnzip' ys $ (max a1 a2, max b1 b2)














