import Data.Char
import Prelude hiding (map, filter)
import Data.List hiding (map, filter)

takeThree = take 3
dropThree = drop 3
hundredTimes = replicate 100

index = ([0..] `zip`)
index' = (`zip` [0..])
divider =  (`replicate` '=')
finishSentence = (++".")
applyToPair :: (a -> b) -> (a, a) -> (b, b)
applyToPair f (x, y) = (f x, f y)

applyOnLast :: (a -> a -> a) -> [a] -> [a] -> a
applyOnLast f xs ys = f (last xs) (last ys)

addThree :: Num a => a -> a -> a -> a   -- or: a -> (a -> (a -> a))
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 = applyOnLast $ addThree 100

applyManyTimes n f x
  | n <= 0 = x
  | otherwise = applyManyTimes (n-1) f (f (x))
  
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

listifyList :: [a] -> [[a]]
listifyList = map (:[])

cutoff :: Int -> [Int] -> [Int]
cutoff n = map $ min n

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
  

sumEvenSquares :: [Integer] -> [Integer]
sumEvenSquares xs = map (^2) $ filter (even) xs

freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (\x -> freq x xs >= n) xs

foo2 :: [Int] -> [Int]
foo2 = map (\x -> (x + 1)^2) 

camelCase''' s = 
  concatMap (\(h:t) -> toUpper h : t) $ words s
  
withinInterval n m xs = filter (\x -> x >= n && x <= m) xs

sndColumn = map (!! 1)
sndColumn' = map (\(_:x:_) -> x)








