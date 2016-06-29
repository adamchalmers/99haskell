import System.Random
import Data.List

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

-- Q27

disjoint :: [a] -> [Int] -> [[[a]]]
disjoint choices sizes =
    djFull choices sizes [map (\_->[]) [0..length sizes - 1]]

djFull :: [a] -> [Int] -> [[[a]]] -> [[[a]]]
djFull choices sizes output = filter (\l -> length (concat l) >= sum sizes)  (dj choices sizes output)


dj :: [a] -> [Int] -> [[[a]]] -> [[[a]]]
dj (choice:choices) sizes output = dj choices sizes output'
    where
        output' = concat $ map (\o -> extend o sizes choice) output
dj [] _ output = output

extend :: [[a]] -> [Int] -> a -> [[[a]]]
extend partial sizes val = map (\i->extend' partial i sizes val) [0..length sizes - 1]
    where
        extend' partial i sizes val = map (f i) (zip [0..length partial - 1] partial)
        f i (j, subset) = if j == i then addToSubset val (sizes !! i) subset else subset

addToSubset :: a -> Int -> [a] -> [a]
addToSubset val size list = if size == (length list) then list else val:list

-- Q28

lsort :: [[a]] -> [[a]]
lsort lists = sortOn length lists
