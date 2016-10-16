-- write a function that takes a list of lengths
-- as numbers returns a list of all rectangles
-- that can be constructed from sides of those lengths
--
--   You can represent a rectangle by the pair of its dimensions
--
rectangles lengths = [(l1, l2) | l1 <- lengths, l2 <- lengths]

-- write a function that takes a list of rectangles
-- and computes the area of each one
--
areas rectangles = [fst dims * snd dims | dims <- rectangles]

-- Use your two functions above to write a function
-- that takes a list of lengths and returns all
-- the possible rectangles paired with their areas
--
rectPairs lengths = zip (rectangles lengths) (areas (rectangles lengths))

-- Find all the rectangles with integers dimensions
-- up to 10 whose perimeter is equal to their area.
-- Do not include any rectangles that are equivalent
-- to one another ((2,10) and (10,2) are the
-- same rectangle).
--
--   (It may be more convenient to *not* use your
--    functions from above)
--

perimetersEqualAreas = [(l, w) | l <- [1..10], w <- [1..10], l <= w, l * w == (2 * (l + w))]

