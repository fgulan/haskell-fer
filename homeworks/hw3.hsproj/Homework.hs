module Homework where
--
import Data.List
import Data.Char
import Data.Bits ( xor )
--

-- Task 01
localMaxima :: [Int] -> [Int]
localMaxima (x1:next@(x2:x3:xs))
  | x1 < x2 && x2 > x3 = x2 : localMaxima next
  | otherwise          = localMaxima next
localMaxima _          = []

-- Task 02
transform :: [(Int, String)] -> [(Char, Int)]
transform ((pt, ls):xs) = transformItem pt ls ++ transform xs
  where
    transformItem' pt ls = map (\l -> (toLower l, pt)) ls  
    transformItem pt ls  = [ (toLower l, pt) | l <- ls ] 
transform _ = []

-- Task 03
rule90 :: [Bool] -> [[Bool]]
rule90 xs = [xs] ++ (rule90 $ rule90Step xs)

rule90Step :: [Bool] -> [Bool]
rule90Step xs = [ xor x1 x2 | x1:_:x2:_ <- tails ([False] ++ xs ++ [False])]

pretty :: [[Bool]] -> String
pretty lines = intercalate "\n" [ prettyLine line | line <- lines]
  where
    prettyLine cells = [ prettyCell cell | cell <- cells ]
    prettyCell True  = '#'
    prettyCell _     = ' '

-- Task 04
f :: [String]
f = f' "1" 
  where
    f' input = [input] ++ (f' $ lookAndSay input)

lookAndSay x 
  | null x = []
  | otherwise = (countNums x) ++ (lookAndSay $ dropWhile (== x !! 0) x)
  where
    countNums x = (show $ length $ takeWhile (== x !! 0) x) ++ [(x !! 0)]

