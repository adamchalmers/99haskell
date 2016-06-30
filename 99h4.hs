prime :: Int -> Bool
prime n = not $ any (\i -> n `mod` i == 0) [2..n-1]

gcd' :: Int -> Int -> Int
gcd' a b = if b == 0 then a else gcd b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b = (gcd' a b) == 1

phi :: Int -> Int
phi m = length $ [ i | i <- [1..(m-1)], coprime m i]

primeFactors :: Int -> [Int]
primeFactors n = if n <= 1 then [] else p:primeFactors(quot n p)
	where
		p = head [i | i <- [2..], n `mod` i == 0 && prime i]