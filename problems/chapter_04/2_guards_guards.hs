-- Write a function that compares two
-- pairs and determines the ordering of them
pairCompare :: Ord a => (a,a) -> (a,a) -> Ordering
pairCompare (x1, y1) (x2, y2)
  | x1 < x2 = LT
  | x1 > x2 = GT
  | y1 < y2 = LT
  | y1 > y2 = GT
  | otherwise = EQ

-- Write a function that implements Fizz Buzz
-- (http://en.wikipedia.org/wiki/Fizz_buzz)
-- for all positive integers using guards.
fizzBuzz :: (Integral a, Show a) => a -> String
fizzBuzz n
  | n `mod` 15 == 0 = "Fizz Buzz"
  | n `mod` 5  == 0 = "Buzz"
  | n `mod` 3  == 0 = "Fizz"
  | otherwise = (show n)
