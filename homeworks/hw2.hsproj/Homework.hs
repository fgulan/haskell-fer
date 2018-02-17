module Homework where
--
import Data.List
import Data.Char
--

-- Task 01

toRNA :: String -> String
toRNA dna = [ nucleotide' n | n <- dna ]

nucleotide' :: Char -> Char
nucleotide' 'G' = 'C'
nucleotide' 'C' = 'G'
nucleotide' 'T' = 'A'
nucleotide' 'A' = 'U'
nucleotide' _   = error "Non-existent nucleotide in DNA"

-- Task 02

multiply :: Int -> Int -> Int
multiply times n
  | times == 0 = 0
  | times == 1 = n
  | times < 0  = - multiply (abs times) n
  | otherwise  = n + multiply (times - 1) n 

divide :: Int -> Int -> Int
divide n divisor
  | divisor == 0         = error "undefined"
  | divisor < 0 && n < 0 = divide (abs n) (abs divisor)
  | divisor < 0          = - divide n (abs divisor)
  | n < 0                = - divide (abs n) divisor
  | divisor == n         = 1
  | divisor > n          = 0
  | otherwise            = 1 + divide (n - divisor) divisor

greatestCD :: Int -> Int -> Int
greatestCD x y = gcd' (abs x) (abs y)
  where
    gcd' a 0 = a
    gcd' a b = b `seq` gcd' b (a `mod` b)
  
-- Task 03

first19 = ["zero", "one", "two", "three", "four", "five", "six", "seven", 
           "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
           "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

numberToWords :: Int -> String
numberToWords 20 = "twenty"
numberToWords 30 = "thirty"
numberToWords 40 = "forty"
numberToWords 50 = "fifty"
numberToWords 60 = "sixty"
numberToWords 70 = "seventy"
numberToWords 80 = "eighty"
numberToWords 90 = "ninety"
numberToWords n 
-- first 19 numbers
  | 0 <= n && n < 20 = first19 !! n
-- tens with numbers
  | 20 < n && n < 30 = "twenty-" ++ numberToWords (n - 20)
  | 30 < n && n < 40 = "thirty-" ++ numberToWords (n - 30)
  | 40 < n && n < 50 = "forty-" ++ numberToWords (n - 40)
  | 50 < n && n < 60 = "fifty-" ++ numberToWords (n - 50)
  | 60 < n && n < 70 = "sixty-" ++ numberToWords (n - 60)
  | 70 < n && n < 80 = "seventy-" ++ numberToWords (n - 70)
  | 80 < n && n < 90 = "eighty-" ++ numberToWords (n - 80)
  | 90 < n && n < 100 = "ninety-" ++ numberToWords (n - 90)
-- hundreds
  | 100 <= n && n < 1000 && n `mod` 100 == 0 = numberToWords (n `div` 100) ++ " hundred"
  | 100 <= n && n < 1000 = numberToWords (n `div` 100) ++ " hundred " ++ numberToWords (n `mod` 100)
-- thousands
  | 1000 <= n && n < 1000000 && n `mod` 1000 == 0 = numberToWords (n `div` 1000) ++ " thousand"
  | 1000 <= n && n < 1000000 = numberToWords (n `div` 1000) ++ " thousand " ++ numberToWords (n `mod` 1000)
-- milions
  | 1000000 <= n && n < 1000000000 && n `mod` 1000000 == 0 = numberToWords (n `div` 1000000) ++ " million"
  | 1000000 <= n && n < 1000000000 = numberToWords (n `div` 1000000) ++ " million " ++ numberToWords (n `mod` 1000000)

-- Task 04

undefined' :: a
undefined' = error "My Undefined"
