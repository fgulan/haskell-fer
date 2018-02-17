import Data.Char
import Prelude hiding (flip,curry)
import Data.List 
import Control.Monad
import Data.Ord (comparing)

succOfFst :: (Int, Int) -> Int
succOfFst = succ . fst

applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

caesarCode :: String -> String
caesarCode = map succ . filter (/=' ')

wordSort :: String -> String
wordSort = unwords . sort . words

initials = map toUpper . map head . words
initials2 = map (toUpper . head) . words

sumEven :: [Integer] -> Integer
sumEven = sum . map (snd) . filter (even . fst) . zip [0..]

filterWords :: [String] -> String -> String
filterWords xs = unwords . filter (\x -> not $ elem x xs) . words

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

diffs :: [Int] -> [Int]
diffs xs =  map (uncurry (-)) $ zip xs $ tail xs

maxDiff :: [Int] -> Int
maxDiff = maximum . diffs

minDiff :: [Int] -> Int
minDiff = minimum . diffs

maxMinDiff :: [Int] -> (Int, Int)
maxMinDiff xs = (maxDiff xs, minDiff xs)

maxScore xs = maximum $ map (snd) xs

studentsPassed :: [(String, Float)] -> [String]
studentsPassed xs = 
  let limit = 0.5 * (maximum $ map (snd) xs) in
    map (fst) $ filter (\x -> snd x >= limit) xs 
   
isTitleCased :: String -> Bool
isTitleCased = all (isUpper . head) . words

elem' a = foldr (\x acc -> acc || (x == a)) False
reverse' xs = foldr (\y acc -> acc ++ [y]) [] xs
nubRuns xs = foldr (
  \y acc -> acc ++
    if (not (null acc) && y == (last acc)) then
      []
    else 
      [y]
  ) [] xs




reverse'' = foldl (\acc y -> y:acc) []

sumEven' :: [Integer] -> Integer
sumEven' xs = foldl (\acc (i, el) -> if even i then acc + el else acc) 0 (zip [0..] xs)

maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "No go"
maxUnzip xs = foldl1 (\(a1, b1) (a2 ,b2) -> (max a1 a2, max b1 b2)) xs





