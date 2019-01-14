-- Use folds to solve the following problems

-- Define a function that determines to total area of a list of rectangles
totalArea :: (Num a) => [(a,a)] -> a
totalArea = foldl (\acc (l, w) -> acc + l * w) 0

-- Define a function that finds the dimensions of a rectangle that will
-- encompass every rectangle in a list (individually, not at all the same
-- time)
encompass :: (Ord a) => [(a,a)] -> (a,a)
encompass = foldl1 ( \(ll, ww) (l, w) -> (max ll l, max ww w))

-- Define a function that determines the total magnitude (abs) of
-- multiples of 3 in a list. Do so only via fold, not using map or filter.
amountOfFizz :: [Integer] -> Integer
amountOfFizz =
  foldl (\acc x -> acc + (if fizz x then 1 else 0)) 0
    where fizz x = x `mod` 3 == 0

-- Define a function that determines if any rectangle in
-- a given list is big. A rectangle is big if its area is greater than 100.
anyBig :: (Num a, Ord a) => [(a,a)] -> Bool
anyBig = foldl (\acc (l, w) -> acc || l * w > 100) False
-- Mine works, but the answer key has a nicer idiom.
-- The way to do any is to fold over (||)
--anyBig xs = foldl (||) (map (\(l, w) -> l * w > 100) xs)


-- Define a function that flattens a list of pairs
flatten :: [(a,a)] -> [a]
flatten = foldl (\acc (a,b) -> a:b:acc) []

-- Define a function that places a comma between every two strings in a
-- list of strings.
-- e.g. ["Hello", "Bob", "and Alice"] -> ["Hello", ",", "Bob", ",", "and Alice"]
separate :: [String] -> [String]
separate xs = tail $ foldr (\x acc -> ", ":x:acc) [] xs

-- Define a function that joins together a list of strings
join :: [String] -> String
join = foldl1 (++)

-- Define a function that returns every suffix of a string
suffixes :: String -> [String]
suffixes = scanr (:) "" --(\(x, acc) -> x:acc)
-- It's the cons operator. I had originally written it like the comment

-- Define a function that returns every prefix of a string
prefixes :: String -> [String]
prefixes = scanl (\acc x -> acc ++ [x]) ""
