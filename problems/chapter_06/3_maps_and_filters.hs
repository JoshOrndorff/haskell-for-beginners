-- Use map and filter to construct expressions
-- for the following calculations.

-- Double all the numbers from 1 to 10
doubles = map (*2) [1..10]

-- Compute the first 20 perfect cubes.
twentycubes = map (^3) [1..20]

-- Greet [Bob, Alice, and Eve] by adding "Hello, " in front each name.
hellos = map ("Hello, " ++) ["Bob", "Alice", "Eve"]

-- Find all the odd numbers in [2,13,7,14,4,15,6,8,9,11]
someOdds = filter odd [2,13,7,14,4,15,6,8,9,11]

-- Find all the even perfect cubes greater than 30 up to 1000
evenCubes = ((filter (\x -> even x && (>30) x)) . (map (^3))) [1..10]

-- Create a list of all the positive even numbers (using filter)
positiveEvens = filter even [1..]

-- Create a list of all the positive even numbers (using map)
positiveEvens' = map (*2) [1..]

-- Find all the perfect squares less than 10,000
firstHundredSquares = map (^2) [1..100]

-- Find all the proper divisors of 8128. These are the positive
-- integers that divide it evenly and are less than it.
divisors =
  filter (divides 8128) [1..8127]
    where divides a b = a `mod` b == 0
