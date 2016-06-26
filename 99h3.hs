import System.Random

-- PREVIOUS 

slice xs i j = [v | (n,v) <- (zip [1..] xs), n >= i && n <= j]
removeAt n l = (gone, residue)
    where
        gone = l !! n
        sl = slice l
        residue = (sl 0 n) ++ (sl (n+2) (length l))

-- Q21
insertAt :: a -> [a] -> Int -> [a]
insertAt val l i = (slice l 0 (i-1)) ++ [val] ++ (slice l i (length l))

-- Q22
range :: Int -> Int -> [Int]
range low high = reverse $ range' low high []
    where
        range' low high output = 
            if low == high
            then (low:output)
            else range' (low+1) high (low:output)

-- Q23
rndSelect :: [a] -> Int -> Int -> [a]
rndSelect l n seed = rndLoop (mkStdGen seed) l n []

rndLoop :: StdGen -> [a] -> Int -> [a] -> [a]
rndLoop _ _ 0 out = out
rndLoop gen l n out = rndLoop gen' l' (n-1) (val:out)
    where
        (gen', l', val) = rmRnd gen l

rmRnd :: StdGen -> [a] -> (StdGen, [a], a)
rmRnd gen l = (gen', l', removed)
    where
        (n, gen') = randomR (0, length l) gen
        (removed, l') = removeAt n l

-- Q24
diffSelect :: Int -> Int -> Int -> [Int]
diffSelect m n seed = rndSelect [0..m] n seed

-- Q25 
rndPermute :: [a] -> Int -> [a]
rndPermute l seed = map (\i -> l !! i) (diffSelect n n seed)
    where
        n = length l

-- Q26
combinations :: [a] -> Int -> [[a]]
combinations set k = reverse $ filter (\l -> length l == k) (powerset set)

powerset :: [a] -> [[a]]
powerset l = map reverse (ps l [])
    where
        ps :: [a] -> [[a]] -> [[a]]
        ps [] output = output
        ps (s:rest) [] = ps rest [[s], []]
        ps (s:rest) output = ps rest ([(s:o) | o <- output] ++ output)

