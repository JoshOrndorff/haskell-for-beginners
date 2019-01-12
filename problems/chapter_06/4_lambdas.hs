-- Define the following funtions using lambda
-- notation.

timesTwo :: Num a => a -> a
timesTwo = \x -> 2 * x

triangleArea :: Fractional a => a -> a -> a
triangleArea = \b h -> 1/2 * b * h

jamesBondIntroduction :: String -> String -> String
jamesBondIntroduction = \f l -> l ++ "... " ++ f ++ " " ++ l

rectangularVolume :: Num a => a -> a -> a -> a
rectangularVolume = \w l h -> w * l * h

-- Use lambdas to perform the following maps and filters

-- Compute the areas of these rectangles: [(1,2),(3,4),(5,6)]
rectAreas = map (\(l, w) -> l * w) [(1,2),(3,4),(5,6)]

-- Pick out all the vowels from "Bond, James Bond"
bondVowels = filter ((flip elem) "aeiouAEIOU") "Bond, James Bond"

-- Write a function to pick out all the numbers in a list greater
-- than a given number
greaterThanN thresh = filter (\x -> x > thresh)
