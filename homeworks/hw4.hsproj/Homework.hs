module Homework where
--
import Data.List
import Data.Char
import Data.Function ( fix )
--

-- Task 01

-- non accumulator style
factorial :: (Num a, Eq a) => a -> a
factorial = fix (\rec x -> if x == 0 then 1 else x * rec (x - 1))

-- non accumulator style
sum' :: Num a => [a] -> a
sum' = fix (
  \rec ys -> 
    if null ys then 
      0 
    else 
      head ys + (rec $ tail ys)
  )

-- accumulator style
factorial' :: (Num a, Eq a) => a -> a
factorial' n = fix (
  \rec (x, n) ->
    if x == 0 then 
      n
    else 
      rec (x - 1, x * n)
  ) (n, 1)

-- accumulator style
sum'' :: Num a => [a] -> a
sum'' xs = fix (
  \rec (ys, acc) -> 
    if null ys then 
      0 
    else 
       rec (tail ys, acc + (head ys))
  ) (xs, 0)

nats :: [Integer]
nats = fix (\rec x -> x:(rec (x + 1))) 1

map' :: (a -> b) -> [a] -> [b]
map' f = fix (
  \rec xs -> 
    if null xs then 
      [] 
    else 
      (f $ head xs):rec (tail xs)
  )
  
zip' :: [a] -> [b] -> [(a, b)]
zip' xs ys = fix (
  \rec (xs, ys) -> 
    if null xs || null ys then 
      [] 
    else 
      (head xs, head ys):(rec (tail xs, tail ys))
  ) (xs, ys)
  
-- Task 02
subsets :: Eq a => Int -> [a] -> [[a]]
subsets 0 _  = [[]]
subsets _ [] = []
subsets n xs = subsets' n $ nub xs 
  where subsets' n (x:xs) = map (x:) (subsets (n-1) xs) ++ subsets n xs 

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = concatMap (extend x) $ partitions xs

extend :: a -> [[a]] -> [[[a]]]
extend x [] = [[[x]]]
extend x (y:ys) = ((x:y):ys) : map (y:) (extend x ys)
    
-- Task 03
permutations' :: [a] -> [[a]]
permutations' = undefined
