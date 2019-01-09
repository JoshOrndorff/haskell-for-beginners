-- For all the functions you define in these problems,
-- write the type signature before you start writing
-- the definition.

-- Define a function that applies another function
-- to each element of a pair.
pairMap :: (a -> b) -> (a, a) -> (b, b)
pairMap f (x, y) = (f x, f y)


-- Use your function above to write a function
-- that scales (multiplies) a pair by a given
-- factor.
scale :: Int -> (Int, Int) -> (Int, Int)
scale factor = pairMap (* factor)


-- Define a function that applies a function to
-- each element of a list.
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- Another idea from published solutions
-- Uses list comprehension
-- map' f xs = [f x | x <- xs]

-- Use your functions above to define a function that
-- scales every pair in a list of pairs by ten.
scalePairsByTen :: [(Int, Int)] -> [(Int, Int)]
scalePairsByTen = map' (scale 10)

-- Define a function that accepts a function and an
-- initial value. It should return the list of values
-- produced by repeatedly applying the function to its
-- own return value.
generations :: (a -> a) -> a -> [a]
generations f x = fx : generations f fx
  where fx = f x


-- Use your functions above to define a function that
-- takes a pair and returns a list of all (infinitely many)
-- pairs by scaling that pair by powers of ten.
scaledByPowersOfTen :: (Int, Int) -> [(Int, Int)]
scaledByPowersOfTen = generations (scale 10)
