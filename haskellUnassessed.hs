import Data.Char
import Data.List

-- Functions

addDigit :: Integer -> Integer -> Integer
addDigit x y = x*10 + y

celcToFahr :: Double -> Double
celcToFahr x = x * 9/5 + 32

type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance (x1,y1) (x2,y2) = sqrt (abs (x2 - x1)^2 + abs (y2 - y1)^2)

triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea (x1,y1) (x2,y2) (x3,y3)
    = sqrt ((s) * (s - a) * (s - b) * (s - c))
    where a = distance (x1,y1) (x2,y2)
          b = distance (x2,y2) (x3,y3)
          c = distance (x1,y1) (x3,y3)
          s = (a + b + c) / 2

isPrime :: Int -> Bool
isPrime 1 = error("1 is not prime")
isPrime a = length [x | x <- [2..a], a `mod` x == 0] == 2

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

perm :: Int -> Int -> Int
-- pre n > 0 && r > 0 && n >= r
perm _ 0 = 1
perm n r = n * perm (n-1) (r-1)

choose :: Int -> Int -> Int
choose n r
    | n == r    = 1
    | otherwise = choose (n-1) r * (n `div` (n - r))

remainder :: Int -> Int -> Int 
remainder x y
    | x >= y = remainder (x - y) y
    | otherwise = x

quotient :: Int -> Int -> Int
quotient x y
    | x >= y = 1 + (quotient (x - y) y) 
    | otherwise = 0

binary :: Int -> Int
binary n
    | n < 2     = n
    | otherwise = (binary (n `div` 2))*10 + (n `mod` 2)

add :: Int -> Int -> Int
add x 0 = x
add x y = add (succ x) (pred y)

larger :: Int -> Int -> Int
larger x 0 = x
larger x y 
    | x == y       = x
    | (succ x) > y = x
    | otherwise    = y

chop :: Int -> (Int,Int)
chop x
    | x < 10    = (0,x)
    | otherwise = (1 + q, r)
    where
        (q, r) = chop (x - 10)      

--concatenate :: Int -> Int -> Int
--concatenate x y
--    | y == 0    = x
--    | otherwise = addDigit (concatenate x a) b
--   where 
--        (a,b) = chop y


-- Lists

precedes :: (Ord a) => [a] -> [a] -> Bool
precedes x y
    | x < y     = True
    | x == y    = True
    | otherwise = False

pos :: Int -> [Int] -> Int
pos a (x:xs)
    | a == x = 0
    | otherwise = 1 + pos a xs

twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (x:xs) = (x `elem` xs) || twoSame xs

-- complexity = n^2

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev (xs) ++ [x]

-- complexity = n^2

rev' :: [a] -> [a]
rev' xs
    = rev' xs []
    where 
        rev' [] a = a
        rev' (x:xs) a = rev' xs (x:a) 

-- complexity = n

prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix x string
    | x == take (length x) string = True
    | otherwise          = False

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes x = x : (suffixes (tail x))

substring :: String -> String -> Bool
substring _ [] = False
substring x string
    | prefix x string || elem x (suffixes x) = True
    | otherwise = substring x (tail string)



-- List comprehensions

findAll x t = [y' | (x', y') <- t, x' == x]

remove :: Eq a => a -> [(a, b)] -> [(a, b)] 
remove x pairs
    = [(k, v) | (k, v) <- pairs, x /= k]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

-- using filter
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let smallerSorted = quicksort'(filter (<= x) xs)
        biggerSorted = quicksort' (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

allSplits :: [a] -> [([a], [a])]
allSplits list
    = [splitAt n list | n <- [1..length list - 1]]

prefixes :: [t] -> [[t]]
prefixes [] = [[]]
prefixes (x:xs) = [x] : [x:prefs | prefs <- prefixes xs]

substrings :: String -> [String]
substrings [] = [[]]
substrings str = [i | t <- tails str, i <- tail (inits t)]

perms :: [a] -> [[a]]
perms [] = [[]]
perms xs = [x : ps | x <- xs, ps <- perms (xs \\ [x])]

-- routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
