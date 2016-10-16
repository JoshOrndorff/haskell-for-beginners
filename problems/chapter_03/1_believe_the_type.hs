-- Guess the types of the following definitions.
-- Check your guesses in GHCI as you go.

a :: Char
a = 'a'

b :: Bool
b = False

c :: Integer
c = 100

d :: Double 
d = 10.5

e :: (Integer, [Char])
e = (100, "Pants")

f :: ()
f = ()

g :: [Integer]
g = [ x*x | x <- [1..10] ]

h :: [Char] -> [Char]
h x = "Hello " ++ x

i :: [Char] -> [Char] -> [Char]
i x y = x ++ y ++ " Goodbye"

j :: Char -> [Char] -> [Char]
j x y = x:y ++ " Goodbye"

-- Add type signatures to the following function
-- definitions. If you can load the file into GHCI
-- without an error, then you have a valid signature!

add1 :: Integer -> Integer
add1 x = x + 1

hasThree :: [Integer] -> Bool
hasThree ns = 3 `elem` ns

hasFoo :: [String] -> Bool
hasFoo words = "foo" `elem` words

interject :: String -> String -> String
interject before after = before ++ " - HEY! - " ++ after

isTwice :: Int -> Int -> Bool
isTwice x y = x == y * 2

numberThem :: String -> [(Integer, Char)] -- Can't be Int because it is in infinite list
numberThem word = zip [1..] (word ++ " more")

-- Write functions for the following type signatures.
-- Try to guess what the function should do based
-- on the name and type signature.

area :: Double -> Double -> Double
area x y = x * y

doubleThem :: [Int] -> [Int]
doubleThem xs = [2 * x | x <- xs]

numberOfMs :: String -> Int
numberOfMs s = [a | c <- s, c `elem` "mM"]

-- Write type signatures *and* definitions for the following
-- functions. Guess what they should do based on the name and parameters.
-- In some cases, more than one definition or signature might be acceptable.

removeVowels :: String -> String
removeVowels string = [c | c <- string, not (c `elem` "aeiouAEIOU")]

hypotenuse :: Double -> Double -> Double
hypotenuse a b = sqrt (a * a + b * b)

makeRects :: [Integer] -> [Integer] -> (Integer, Integer)
makeRects lengths widths = [(l, w) | l <- lengths, w -< widths]


