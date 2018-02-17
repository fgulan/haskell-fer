module Homework where
--
import Data.List hiding (insert, lookup)
import Data.Char
import Prelude hiding (insert, lookup)
--

-- Task 01

isLeapYear :: Int -> Bool
isLeapYear year 
  | yearDivBy 400 = True
  | yearDivBy 100 = False
  | yearDivBy 4   = True
  | otherwise     = False
  where yearDivBy x = year `mod` x == 0
  
leapList :: [Int]
leapList = [year | year <- [1996..2017], isLeapYear year]

-- Task 02

powers :: Double -> [Double]
powers value = iterate (value*) 1

evaluate :: Double -> [Double] -> Double
evaluate value coefs = sum $ zipWith (*) coefs $ powers value
 
factorial :: Double -> Double
factorial 0 = 1
factorial value = value * factorial (value - 1)

maclaurin :: [Double]
maclaurin = [1.0 / factorial x | x <- [0..]]

exp' :: Double -> Double
exp' x = evaluate x $ take 170 maclaurin

-- Task 03

findItem :: [(String, a)] -> String -> [(String, a)]
findItem dict key = take 1 $ filter ((==key).fst) dict 

contains :: [(String, a)] -> String -> Bool
contains dict key = not $ null $ findItem dict key

lookup :: [(String, a)] -> String -> a
lookup dict key = 
  case findItem dict key of
    []       -> error "No pair with given key!"
    (pair:_) -> snd pair
  

insert :: [(String, a)] -> (String, a) -> [(String, a)]
insert dict pair 
  | contains dict $ fst pair = dict
  | otherwise                = dict ++ [pair]

remove :: [(String, a)] -> String -> [(String, a)]
remove dict key = filter ((/=key).fst) dict 

update :: [(String, a)] -> String -> a -> [(String, a)]
update dict key value 
  | not $ contains dict key = dict
  | otherwise               = remove dict key ++ [(key, value)]

-- Task 04

cosineSimilarity :: String -> String -> Double
cosineSimilarity = undefined
