import Data.Char
import Data.List
fact :: (Eq a, Num a) => a -> a
fact 0 = 1
fact x = x * fact (x-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

-- structural recursion  -> loop replacement

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs (x:xs) = qs ys ++ [x] ++ qs zs
  where ys = [y | y <- xs, y <= x]
        zs = [z | z <- xs, z  > x]
        

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: Num a => [a] -> a
length' [] = 0
length' (x:xs) = x + length' xs

incList [] = []
incList (x:xs) = (x + 1) : incList xs

concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

maximum' [x] = x
maximum' (x:xs) = x `max` maximum' xs

-- EXCERCISE 1

product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf ([]:xss) = headsOf xss
headsOf (xs:xss) = [head xs] ++ headsOf xss

addToList :: Num a => a -> [a] -> [a]
addToList _ [] = []
addToList n (x:xs) = x + n : addToList n xs



--modMult n m xs 
modMult _ _ [] = []
modMult n m (x:xs) = x * n `mod` m : modMult n m xs

addPr :: Num a => [a] -> [a]
addPr xs = addPr' 0 xs
   where addPr' _ []     = []
         addPr' n (x:xs) = x + n : addPr' x xs

numPositives :: (Num a, Ord a) => [a] -> Int
numPositives []     = 0
numPositives (x:xs) | x > 0     = 1 + numPositives xs
                    | otherwise = numPositives xs

eqTrips [] = []
eqTrips ((a, b, c):xs) | (a == b) && (b == c) = (a, b, c) : eqTrips xs
                       | otherwise            = eqTrips xs
                    

replicate' :: Int -> a -> [a]
replicate' n a | n > 0 = [a] ++ replicate' (n-1) a
               | otherwise = []

-- less correct
replicate2 :: Int -> a -> [a]
replicate2 0 a = []
replicate2 n a = [a] ++ replicate2 (n-1) a

drop' :: Int -> [a] -> [a]
drop' n (x:xs) | n > 0 = drop' (n-1) xs
               | otherwise = x:xs

drop'' :: Int -> [a] -> [a]
drop'' n (x:xs) | n > 0 = drop'' (n-1) xs
                | n < 0 = reverse $ drop'' (abs n) $ reverse $ x:xs
                | otherwise = x:xs

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]





