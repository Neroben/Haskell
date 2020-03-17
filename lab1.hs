max3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = if x > y then
		if x > z then x else z
		else if y > z then y else z

min3 :: Integer -> Integer -> Integer -> Integer
min3 x y z = if x < y then
		if x < z then x else z
		else if y < z then y else z

sort2 :: Integer -> Integer -> [Integer]
sort2 x y = if x > y then [y] ++ [x]
		else [x] ++ [y]

bothTrue :: Bool -> Bool -> Bool
bothTrue x y = if x then
		if y then True else False
		else False

solve2 :: Double -> Double -> (Bool, Double)
solve2 a b = if a == 0 then (False, 0.0)
		else (True, (-1)*b/a)

isParellelSum

isParellel (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> Boolean
isParellel a b c d = 