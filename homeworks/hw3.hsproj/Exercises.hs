{-# LANGUAGE NoMonomorphismRestriction #-}
--
module Exercises where
--
import Data.List
import Data.Char
--

{-
    Here you should provide your solutions to in-class exercises.

    Make sure that ALL FUNCTIONS (including exXXX) have correct TYPE SIGNATURES.

    You should include solutions from following lectures :
    - http://www.fer.unizg.hr/_download/repository/puh-2017-lecture-06.lhs

    DON'T change function names, just remove 'undefined' and write your own
    definition for that function.
-}

{-LECTURE 06-} -- http://www.fer.unizg.hr/_download/repository/puh-2017-lecture-06.lhs

-- EXERCISE 01 =======================================================================

{-
  1.1.
  - Write an accumulator-style recursive definition of
    length' :: [a] -> Int
-}

ex611 = length'
length' :: [a] -> Int
length' xs = len xs 0
  where len []     n = n
        len (x:xs) n = let a = n+1 in a `seq` len xs a

{-
t  1.2
  - Write an accumulator-style recursive definition of
      maxUnzip :: [(Int, Int)] -> (Int, Int)
    that returns the maximum element at the first position and the maximum
    element at the second position in a pair, i.e., it's equivalent to:
      maxUnzip zs = (maximum xs, maximum ys)
        where (xs,ys) = unzip zs
    If the list is empty, return an "empty list" error.

  - Now write a standard recursive definition (without an accumulator).
-}
ex612 = maxUnzip
maxUnzip :: [(Int, Int)] -> (Int, Int)
maxUnzip [] = error "empty list"
maxUnzip (x:xs) = maxUnzip' xs x
  where maxUnzip' [] pair = pair
        maxUnzip' ((a1, b1):ys) (a2, b2) = maxUnzip' ys $ (max a1 a2, max b1 b2)
        
ex662' = maxUnzip'
maxUnzip' :: [(Int, Int)] -> (Int, Int)
maxUnzip' xs 
   | null xs = error "empty List"
   | otherwise =  (maximum $ maxLeft xs, maximum $ maxRight xs)
            where maxLeft [(x1,x2)] = [x1]
                  maxLeft ((x1, x2):xs) = x1: maxLeft xs
                  maxRight [(x1,x2)] = [x2]
                  maxRight ((x1, x2):xs) = x2: maxRight xs 
