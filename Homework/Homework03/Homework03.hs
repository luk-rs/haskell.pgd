-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly.
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).

usage consumption hours = consumption * hours * 30

checkConsumptionMonthly monthly allowed
  | monthly > allowed = "bigger"
  | monthly < allowed = "smaller"
  | otherwise = "equal"

-- checkConsumption consumption hours allowed =
--   let monthly = usage consumption hours
--    in checkConsumptionMonthly monthly allowed
checkConsumption consumption hours = checkConsumptionMonthly monthly
  where
    monthly = usage consumption hours

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.

checkConsumption' consumption hours allowed
  | diff < 0 = "savings " ++ show diff
  | diff > 0 = "excess " ++ show diff
  | otherwise = "exact consumption"
  where
    monthly = usage consumption hours
    diff = monthly - allowed

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.

quotient dividend divisor
  | divisor == 0 = "cannot divide by zero"
  | dividend < divisor = show (dividend / divisor)
  | otherwise = "it's greater than one"

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of square roots for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
complex x y = productsSquare + quotientSquare
  where
    productsSquare = let product = x * y in sqrt product
    quotientSquare = let quotient = x / y in sqrt quotient

complex' x y =
  let productsSqrt = sqrt product
      quotientSqrt = sqrt quotient
   in productsSqrt + quotientSqrt
  where
    product = x * y
    quotient = x / y