import System.Environment
import Text.Printf

-- Q1
myLast :: [a] -> a
myLast = foldr1 (\_ x -> x)

myLast' :: [a] -> a
myLast' = head . reverse

-- Q2
lastButOne :: [a] -> a
lastButOne = head . tail . reverse

-- Q3
seek :: Int -> [a] -> a
seek k = head . (map fn) . (filter predicate) . (zip [1..])
    where 
        predicate (i,v) = i==k
        fn (i,v) = v

seek' :: Int -> [a] -> a
seek' k xs = head $ drop (k-1) xs

-- Q4
myLength :: [a] -> Int
myLength = foldr (\val acc -> acc + 1) 0

-- Q5, slow
myReverse :: [a] -> [a]
myReverse = foldr (\val acc -> acc++[val]) [] 

-- Q5, fast
myReverse' :: [a] -> [a]
myReverse' l = rev l []
    where
        rev [] partial = partial
        rev (x:xs) partial = rev xs (x:partial)

-- Q6
isPalin :: (Eq a) => [a] -> Bool
isPalin x = x == (reverse x)

isPalin' :: (Eq a) => [a] -> Bool
isPalin' x = length (filter (\(i,j) -> i /= j) (zip x (reverse x))) == 0

-- Q7
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- Q8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:(y:rest)) = if x==y then compress (y:rest) else x : compress (y:rest)

compress' :: (Eq a) => [a] -> [a]
compress' = foldr f []
    where
        f x []      = [x]
        f x partial = 
            if x == head partial
                then partial
                else x:partial

-- Q9
pack :: (Eq a) => [a] -> [[a]]
pack = foldr f []
    where
        f x [] = [[x]]
        f x partial@(l:ls) = 
            if x == head l 
            then (x:l):ls
            else [x]:partial

-- Q10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)) . pack

-- Benchmarking

bigString :: Int -> [Char]
bigString n = foldr (\_ a -> a++alphabet) [] [0..n]
    where
        alphabet = "ABCDEFGHIJKLKMNOPQRSTUVWXYZ"

main = do
    printf "%s\n" (myReverse' (bigString 500))
