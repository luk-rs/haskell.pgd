import Data.List (elemIndex)

-- Question 1
-- Investigate the `Bounded` type class. What behaviours it provides?
{-
it is useful to get the min and max values for a given type
-}

-- Question 2
-- The types Int and Word bellong to the same type classes. What is the difference
-- between them? Check maybe the maxBound and minBound parameter for both types.
{-
ghci> [maxBound :: Int,minBound :: Int]
[9223372036854775807,-9223372036854775808]
ghci> [maxBound :: Word,minBound :: Word]
[18446744073709551615,0]
-}

-- Question 3
-- Investigate the `Enum` type class. What behaviours provides?

-- Question 4
-- Add the most general type signatures possible to the functions below.
-- Then uncomment the functions and try to compile.

f1 :: (Fractional a, Show a) => a -> a -> String -> String
f1 x y z = show (x / y) ++ z

f2 :: (Bounded a, Eq a, Enum a) => a -> a
f2 x = if x == maxBound then minBound else succ x

-- Question 5
-- Investigate the numeric type classes to figure out which behaviors they provide to change between numeric types.

sqr :: (Show b, Num b) => b -> [Char] -> [Char]
sqr x b = (++) b $ show . (^) x $ 3

mistery1 :: (Ord a, Fractional b) => a -> a -> b -> b
mistery1 x y z = if x > y then z else z / 2

mistery2 :: (Ord orders, Num orders, Fractional res) => orders -> orders -> res -> res
mistery2 x y z = if x + 1 > y then z else z / 2
