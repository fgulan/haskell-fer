import Data.Char
import Data.List

x = 2

inc x = x + 1

digits2number x y = x * 10 + y

y = inc 2
z = digits2number 4 2
t = max (max 1 4) 2
w = 25 `div` 2
name = "Humpty Dumpty"
s = "One " ++ "two " ++ "three"

condDec x = if x > 0 then x - 1 else x

foo x = (if even x then x*2 else 2) + 1
foo_ x = if even x then x*2 else 2 + 1

bigNumber x = if x >= 1000 then True else False
bigNumberNormal x = x >= 1000

merge s1 s2 = s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2


-- GUARDS

merge3 s1 s2
  | s1 < s2 = s1 ++ " is not " ++ s2
  | otherwise = s1 ++ " is " ++ s2 

grade score | score < 50 = 1
            | score < 63 = 2
            | score < 76 = 3
            | score < 89 = 4
            | otherwise  = 5

-- show Int -> String

showSalary amount bonus 
  | bonus /= 0 = "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
  | otherwise  = "Salary is " ++ show amount

-- 1.1

concat3 s1 s2 s3
  | length s2 < 2 = s1 ++ " " ++ s3
  | otherwise = s1 ++ " " ++ s2 ++ " " ++ s3

-- 1.2

showSalary2 amount bonus = if bonus /= 0 
                      then "Salary is " ++ show amount ++ ", and a bonus " ++ show bonus
                      else "Salary is " ++ show amount
                      

-- Lists --

l1 = [1, 2, 3]
l1' = 1:2:3:[]
l1'' = (1:(2:(3:[]))) -- cons operator 

{-| List memory representation
       :
     /   \
    1     :
        /   \
       2     :
           /   \
          3    []
-}

-- concatenation 

l2 = [1, 2, 3] ++ [4, 5, 6]

listify x = [x]
listify' x = x:[]

-- head tail init last

headL1 = head l1

tailL1 = tail l1 -- list of elements, time consuimg since need to traverse whole tree
lastL1 = last l1 -- last element


l3 = take 3 [9, 2, 10, 3,4]
l4 = drop 3 [9, 2, 10, 3, 4]

l5 = reverse [1, 2 ,3]

l6 = "this is a list"

l7 = head l6

l8 = 'H' : 'a' : "skell"
isPalindrome s = s == reverse s

-- Haskel data analysys cock book


