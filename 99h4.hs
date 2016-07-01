import Data.List
import Data.Map

--Q31
prime :: Int -> Bool
prime n = not $ any (\i -> n `mod` i == 0) [2..n-1]

--Q32
gcd' :: Int -> Int -> Int
gcd' a b = if b == 0 then a else gcd b (a `mod` b)

--Q33
coprime :: Int -> Int -> Bool
coprime a b = (gcd' a b) == 1

--Q34
phi :: Int -> Int
phi m = length $ [ i | i <- [1..(m-1)], coprime m i]

--Q35
primeFactors :: Int -> [Int]
primeFactors 1 = [1]
primeFactors 0 = []
primeFactors n = sort $ p:primeFactors(quot n p)
	where
		p = head [i | i <- [2..], n `mod` i == 0 && prime i]

-- Q36
factorize :: Int -> [(Int, Int)]
factorize n = assocs $ Data.List.foldr acc (empty :: Map Int Int) (primeFactors n)
	where
		acc k m = alter (\_->Just ((+1) $ findWithDefault 0 k m)) k m

factorize' :: Int -> [(Int, Int)]
factorize' n = Data.List.map (\xs -> (head xs, length xs)) (group $ primeFactors n)
