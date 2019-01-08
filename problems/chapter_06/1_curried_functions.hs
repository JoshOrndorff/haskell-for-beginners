-- Add parentheses to the following function definitions
-- to express the curried nature of the functions used,
-- but preserve the functions' behavior.

abc = (take 3) "abcdefg"
triples = ((zip3 "abc") [1,2,3]) [10.0, 12.0, 14.0]
ten = ((*) 5) 2
ten' = (5 *) 2


-- Add type declarations to the following functions.
-- Use parentheses to explicitly express the curried
-- nature of the functions.
and :: Bool -> (Bool -> Bool)
and a b = a && b

volume :: Num a => a -> (a -> (a -> a))
volume a b c = a * b * c

addition :: Num a => a -> (a -> a)
addition = (+)

-- Implement the following functions using partial
-- application and sections.

take10 :: [a] -> [a]
take10 = take 10

tenTimes :: a -> [a]
tenTimes = take10 . repeat -- or replicate 10

timesTen :: Num a => a -> a
timesTen = (*10)

-- A list containing N copies of the integer 10
-- The trick here is that we want to apply repeat to its second arg
tenNTimes :: Int -> [Integer]
tenNTimes = (`replicate` 10)

-- Grossman method is more general
swapCurry f x y = f y x
tenNTimes' = (swapCurry replicate) 10


-- The makeRecord function below makes triples based on the name, age, and
-- weight information about an individual. Implement the convenience functions
-- below to make specific kinds of records. Use partial application to mention
-- as few variables as possible.

makeRecord :: String -> Int -> Float -> (String, Int, Float)
makeRecord name age weight = (name, age, weight)

namedBob = makeRecord "Bob"
bobAt30Years = makeRecord "Bob" 30
aliceAt30Years = makeRecord "Alice" 30
aged40 = (swapCurry makeRecord) 40
weighs170pt5 n a = makeRecord n a 170.5
