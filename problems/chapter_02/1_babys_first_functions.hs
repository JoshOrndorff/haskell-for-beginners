-- define a function named avg that calculates the
-- average of two numbers
avg x y = (x + y) / 2

-- use the avg function to calculate two different
-- "averages" of the numbers 2, 3 and 4.
--   The "averages" are 2.75 and 3.25
avg1 = avg 2 (avg 3 4)
avg2 = avg 4 (avg 3 2)

-- recalculate the two numbers above using infix notation
avg1_infix = 2 `avg` (3 `avg` 4)
avg2_infix = 4 `avg` (3 `avg` 2)

-- define a function that returns a boolean indicating
-- whether or not an interger is a multiple of 5 *or* 3
--   hint: 6 `mod` 3 is 0
--         6 `mod` 5 is 1
multiple x = (x `mod` 5 == 0) || (x `mod` 3 == 0)


-- define a function that returns a boolean indicating
-- whether or not a interger is a *not* multiple of 5 *and* 3
notMultiple x = not ((x `mod` 3 == 0) && (x `mod` 5 == 0))

-- define a function that works just like the last one
-- but uses a different boolean logic operator
-- (or instead of and, or vice-versa)
notMultiple' x = (x `mod` 3 /= 0) || (x `mod` 5 /= 0)


-- BONUS: define a function that works just like the
-- last one but uses *NO* boolean logic operators at all
notMultiple'' x = x `mod` 15 /= 0

-- define a function that checks to make sure the
-- two (or three) functions above give the same answer for a
-- provided number. It should return a string indicating
-- to the user whether or not the functions matched

allAgree x = if notMultiple x == notMultiple' x && notMultiple x == notMultiple'' x
              then "Congratulations, they all agree!"
              else "Sorry, there must be a bug."

