import System.Environment
import Text.Printf

-- Q1
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast (x:[]) = Just x
myLast (x:xs) = myLast xs

myLast' :: [a] -> a
myLast' = head . reverse

-- Q2
lastButOne :: [a] -> a
lastButOne = head . tail . reverse

-- Q3
seek :: Int -> [a] -> a
seek k xs = head $ map fn (filter predicate zlist)
	where 
		predicate (i,v) = i==k
		zlist = zip [1..] xs
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
		rev [] reversed = reversed
		rev (x:xs) reversed = rev xs (x:reversed)

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

-- Q9
pack :: (Eq a) => [a] -> [[a]]
pack xs = reverse $ pack' xs []
	where
		pack' :: (Eq a) => [a] -> [[a]] -> [[a]]
		pack' [] output = output
		pack' (x:xs) [] = pack' xs [[x]]
		pack' (x:xs) ((h:t):rest) = 
			if x==h 
			then pack' xs ((x:[h]++t):rest)
			else pack' xs ([x]:((h:t):rest))

-- Q10
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\xs -> (length xs, head xs)) (pack xs)

-- Benchmarking

bigString :: Int -> [Char]
bigString n = foldr (\_ a -> a++alphabet) [] [0..n]
	where
		alphabet = "ABCDEFGHIJKLKMNOPQRSTUVWXYZ"

main = do
	printf "%s\n" (myReverse' (bigString 500))

-- pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']