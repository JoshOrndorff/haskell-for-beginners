-- Write a function that implements Fizz Buzz
-- (http://en.wikipedia.org/wiki/Fizz_buzz)
-- up to the number 15 with simple pattern matching.
-- After 15, give up and lose the game.
fizzBuzzLose :: Integer -> String
fizzBuzzLose 1 = "one"
fizzBuzzLose 2 = "two"
fizzBuzzLose 3 = "fizz"
fizzBuzzLose 4 = "four"
fizzBuzzLose 5 = "buzz"
fizzBuzzLose 6 = "fizz"
fizzBuzzLose 7 = "seven"
fizzBuzzLose 8 = "eight"
fizzBuzzLose 9 = "fizz"
fizzBuzzLose 10 = "buzz"
fizzBuzzLose 11 = "eleven"
fizzBuzzLose 12 = "fizz"
fizzBuzzLose 13 = "thirteen"
fizzBuzzLose 14 = "fourteen"
fizzBuzzLose 15 = "fizz buzz"
fizzBuzzLose _ = "You Lose"

-- Write two functions to calculate the 2 and 3 dimensional
-- dot product using tuples and pattern matching:
--   http://en.wikipedia.org/wiki/Dot_product#Algebraic_definition
dot2D :: (Num a) => (a, a) -> (a, a) -> a
dot2D (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

dot3D :: (Num a) => (a, a, a) -> (a, a, a) -> a
dot3D (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

-- Write a function to flip the components of all the pairs in
-- a list.
flipAll :: [(a,b)] -> [(b,a)]
flipAll xs = [(y, x) | (x, y) <- xs]

-- Write a function that moves the first element of a list to the end.
rot1 :: [a] -> [a]
rot1 (x:xs) = xs ++ [x]

-- Write your own implementation of reverse to reverse a list
--   (your function will probably need to call itself)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' xs = last xs : reverse' (init xs)

-- Write a function to tell if a list's length is > 4.
-- Use only pattern matching to get the answer.
longerThan4 :: [a] -> Bool
longerThan4 [] = False
longerThan4 (_:[]) = False
longerThan4 (_:_:[]) = False
longerThan4 (_:_:_:[]) = False
longerThan4 (_:_:_:_:[]) = False
longerThan4 xs = True

-- Write a function that pairs up each member of a list with
-- the one after it. For the list [1,2,3] the function should
-- return the list [(1,2),(2,3)]. (@ can help you here)
pair :: [a] -> [(a, a)]
pair all@(x:xs) = zip all xs

