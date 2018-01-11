import Text.Printf

-- Q12
data Run a = Single a | Multiple Int a deriving (Show)
unRun :: (Eq a) => [Run a] -> [a]
unRun = foldr f []
    where
        f (Single x)     partial = x:partial
        f (Multiple n x) partial = (replicate n x) ++ partial

-- Q13
run :: (Eq a) => [a] -> [Run a]
run = foldr f []
    where
        f x [] = 
            [Single x]
        f x partial@(Single p:ps) = 
            if x == p
            then (Multiple 2 p):ps
            else (Single x):partial
        f x partial@(Multiple n p:ps) =
            if x == p
            then (Multiple (n+1) p):ps
            else (Single x):partial

-- Q14
dupli :: [a] -> [a]
dupli [] = []
dupli (h:t) = [h,h] ++ dupli t 

-- Q15
repli :: Int -> [a] -> [a]
repli _ [] = []
repli n (h:t) = (take n (repeat h)) ++ repli n t

-- Q16
dropMod :: Int -> [a] -> [a]
dropMod n l = map (\(_,v) -> v) $ filter (\(i,val) -> (i `mod` n)==0) $ zip [0..] l

dropMod' :: Int -> [a] -> [a]
dropMod' n l = [v | (i,v) <- (zip [0..] l), i `mod` n == 0]

-- Q17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

split' xs n = split'' xs []
    where
        split'' [] ys = (ys,[])
        split'' (x:xs) ys = 
            if length ys == n
            then (reverse ys, x:xs)
            else split'' xs (x:ys)

-- Q18
slice :: [a] -> Int -> Int -> [a]
slice xs i j = [v | (n,v) <- (zip [1..] xs), n >= i && n <= j]

-- Q19
rotate :: Int -> [a] -> [a]
rotate n l = 
    if n >= 0 
        then (slice l (n+1) (length l)) ++ (slice l 0 n)
        else (slice l (length l + n + 1) (length l)) ++ (slice l 0 (length l + n))

-- Q20
removeAt :: Int -> [a] -> (a, [a])
removeAt n l = (gone, residue)
    where
        gone = l !! (n-1)
        residue = (slice l 0 (n-1)) ++ (slice l (n+1) (length l))

main = do
    printf "Hello, world.\n"