import Control.Applicative
import Data.List

-- Q46

and' :: Bool -> Bool -> Bool
and' x y = x && y

or' :: Bool -> Bool -> Bool
or' x y = x || y

nand' :: Bool -> Bool -> Bool
nand' x y = not (x && y)

nor' :: Bool -> Bool -> Bool
nor' x y = not (x || y)

xor' :: Bool -> Bool -> Bool
xor' x y = or' (and' x (not y)) (and' y (not x))

impl' :: Bool -> Bool -> Bool
impl' x y = or' (not x) y

equ' :: Bool -> Bool -> Bool
equ' x y = and' (impl' x y) (impl' y x)

table :: (Bool -> Bool -> Bool) -> IO [()]
table fn = sequence $ map putStrLn (table' fn)

table' :: (Bool -> Bool -> Bool) -> [String]
table' fn = map (\(a,b) -> (sh a) ++ " | " ++ (sh b) ++ " || " ++ (sh (fn a b))) rows2

sh :: Bool -> String
sh b = if b then "True " else "False"

rows2 :: [(Bool, Bool)]
rows2 = (,) <$> [True, False] <*> [True, False]