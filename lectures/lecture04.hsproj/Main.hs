import Data.Char
import Data.List


-- pattern matching 

magicNumber2 :: Int -> String
magicNumber2 42 = "Yeah"
magicNumber2 x = "Nope, try again" -- magicNumber2 _ = "Nope, try again"  -- when variable is not used

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y


addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


mallcolmInTheMiddle :: (a, b, c) -> b
mallcolmInTheMiddle (_, y, _) = y

leaves :: ((a, a), (a, a)) -> [a]
leaves ((x, y), (z, w)) = [x, y, z, w]

goo (x, 1) = x + 1

head' :: [a] -> a 
head' [] = error "No head to behead"
head' (x:_) = x

headSwap :: [a] -> [a] -> ([a], [a])
headSwap (x:xs) (y:ys) = (y:xs, x:ys)

------- EXERCISE 1

headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter ([]:(x:_):_) = x
headHunter ([]:[]:_) = error "No head to behead" 

firstColumn :: [[a]] -> [a]
firstColumn m = [ h | (h:_) <- m]


triangleArea :: Double -> Double -> Double -> Double
triangleArea a b c = sqrt $ s * (s - a) * (s - b) * (s - c)
  where s = (a + b + c) / 2
  

--------- EXCERCISE 4


pairlist :: Show c => (Int, Int) -> [c] -> String
pairlist p (_:z:_) =
  "The pair " ++ case p of
    (1, 1)  -> "contains two ones "
    (1, _) -> "contains one one"
    (_, 1) -> "contains one one"
    (_, _) -> "does not contains ones" ++  show z




