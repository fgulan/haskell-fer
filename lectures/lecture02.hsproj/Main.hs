import Data.Char
import Data.List

-- Lists are type safe

l8 = repeat 'a'
l9 = cycle [1, 2]
l10 = replicate 10 'a'

l11 = take 5 (repeat 'a')

-- instead f () == f $ -- $ means apply
replicate' n x = take n $ repeat x

l12 = [1..100]
l13 = [1,3..999]
l14 = take 10 [1,3..100]
l15 = [1..] -- infinite list

l16 = ['a'..'z']
l17 = take 10 [1..]
l18 = head [1..]

l19 = tail [1..]
n = length [1..]

trim l = tail (init l)

blanks = repeat ' '

padTo10 s = s ++ take (10 - length s) blanks

l20 = head []

l21 = [[1,2,3], [4,5,6]]
l22 = ["red", "green"]

l23 = concat l22

m1 = minimum [4, 3, 1, 2]
m2 = maximum "Haskel for the win!!"

-- indexing

e1 = [1,3..100] !! 17

e2 = l21 !! 1 !! 2

intToChar i = ['A'..] !! (i - 65)

itc i | i >= 65 = intToChar i
      | otherwise = error "No go"
      
r1 = and [True, True, False]
r2 = or [True, True, False]

l24 = nub [1, 2, 3, 2, 2, 3, 1] -- removes duplicates
l25 = nub "Give me every letter only once!"
l26 = sort [1, 4, 3, 7]

i1 = elem 1 [1,2,3] -- Java shit
i2 = elem 3 [4, 5, 6]

i3 = 6 `elem` [4, 5, 6] -- this is how it should be used in Haskell

isEmpty = null l23 -- always use null





-- ========= EXECERCISE 1

-- 1.1

trim3elems xs = trim $ trim $ trim xs

-- 1.2

initials s1 s2 = [head s1, ' ', '.', ' ', head s2, '.'] 

-- 1.3

concatLongest l1 l2 | length l2 > length l1 = l2 ++ l1
                    | otherwise = l1 ++ l2

-- 1.4

safeHead l | null l = []
           | otherwise = [head l]
           


-- =========== LIST COMPREHENSIONS

doubles = [x * 2 | x <- [1..10]]

               -- outer loop, inner loop, more inner loop....
sums1 = [x + y | x <- [1..10], y <- [1..10]]

sums2 = [x + y | x <- [1..10], y <- [1..10], x < y]

sums3 = [x + y | x <- [1..10], y <- [1..10], x < y, odd x || even y]

lengths xss = [length xs | xs <- xss]

-- x -> valie
-- xs -> list of value
-- xss -> list of lists of values

totalLength xss = sum $ lengths xss
codes = [[c1, c2] | c1 <- "abc", c2 <- "123"]

ceaserCode s = [succ c | c <- s, c /= ' ']

onlyDigits s = [c | c <- s, isDigit c]


-- ========= EXCERCISE

-- 2.1

doublesFromTo a b | b > a = [x * 2 | x <- [a..b]]
                  | otherwise = reverse $ doublesFromTo b a

nextLetter :: Int -> Int -> Int
nextLetter n c
    | c == ord ' ' = ord ' '
    | c + n > ord 'z' = ord 'a' + (n - (ord 'z' - c))
    | otherwise = c + n

newone = nextLetter . ord $ ord 
-- . ord 'a' $ 3


--ceaserCodeBetter n xs = [ chr $nextLetter(ord(toLower(c)) n) | c <- xs, c /= ' ']

letterCount s = totalLength [w | w <- words s, length w > 2]
