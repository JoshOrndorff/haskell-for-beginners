-- Add type declarations to the following definitions.
-- Try different declarations and see which ones GHCI
-- will accept as valid. Try to find the declaration
-- that will allow the definition to work for the most
-- types by using Type Classes.

increment :: (Num a) => a -> a
increment x = x + 1

removeAll :: (Eq a) => a -> [a] -> [a]
removeAll badX xs = [ x | x <- xs, x /= badX ]

sortPair :: (Ord a) => (a, a) -> (a, a)
sortPair pair = if fst pair <= snd pair
                then pair
                else (snd pair, fst pair)

one :: Read a => a
one = read "1"

-- Define pi in several different ways by converting the
-- string "3.14159" using the read function. Create
-- a float version, a double version, and a version
-- that will work for any readable type.
--
-- BONUS: Try to make the polymorphic safer by having
-- it fail with a type error rather than an exception
-- if a type that can't represent pi is requested.

floatPi = read "3.14159" :: Float
doublePi = read "3.14159" :: Double

polymorphicPi :: Read a => a
polymorphicPi = read "3.14159"

safePolymorphicPi :: (Read a, Floating a) => a
safePolymorphicPi = read "3.14159" :: (Read a) => (Num a) => a

-- Define a function (with type declaration)
-- that will accept a value and product a list
-- of the next three values in the series. It
-- should work for any type that supports the
-- succ function

nextThree :: Enum a => a -> [a]
nextThree orig = [succ orig] ++
                 [succ $  succ orig] ++ 
                 [succ $ succ $ succ orig]

-- Define a function (with type declaration)
-- that adds the square of two any numbers.

addSquares :: Num a => a -> a -> a
addSquares x y =  x * x + y * y

-- Define a function (with type declaration)
-- that uses sqrt and your adding squares
-- function from above to calculate the
-- length of the hypotenuse of a triangle.
hypot :: Floating a => a -> a -> a
hypot a b = sqrt $ addSquares a b


-- Define a funtion (with type declaration)
-- and takes two values and returns a string
-- showing the values separated by ": ", as
-- if they were key and value in a dictionary.
pair :: (Show a, Show b) => a -> b -> String
pair key value = show key ++ ": " ++ show value

-- Define a polymorphic constant that will
-- give all the possible values of a type that
-- is both bounded and enumerable.

allAllowed :: (Bounded a, Enum a) => [a]
allAllowed = [minBound..maxBound]

-- BONUS: Define new versions of your squaring
-- and hypotenuse functions that allow the
-- parameters to be of *different* types. You can
-- assume the values are able to be converted to
-- integers.

