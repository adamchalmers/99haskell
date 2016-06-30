import Control.Applicative
import Data.List

-- Q47

ands :: [Bool] -> Bool
ands xs = all (\b->b) xs

ors :: [Bool] -> Bool
ors xs = any (\b->b) xs

nands :: [Bool] -> Bool
nands xs = not $ ands xs

nors :: [Bool] -> Bool
nors xs = not $ ors xs

xors :: [Bool] -> Bool
xors xs = ands [(not $ ands xs), (not $ nors xs)]

rows :: Int -> [[Bool]]
rows 1 = [[True], [False]]
rows n = concat $ map (\vals->[True:vals, False:vals]) (rows $ n-1)

sh :: Bool -> String
sh b = if b then "True " else "False"

table :: Int -> ([Bool] -> Bool) -> [String]
table n fn = map (\vals -> (header vals) ++ " || " ++ (sh (fn vals))) (rows n)
	where
		header vals = takeLastCol (concat $ map (\b -> (sh b) ++ " | ") vals) 
		takeLastCol s = take (length s - 3) s

printTable :: Int -> ([Bool] -> Bool) -> IO [()]
printTable n fn = sequence $ map putStrLn (table n fn)
