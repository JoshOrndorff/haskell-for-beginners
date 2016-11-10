-- Write a function to tell whether a triangle
-- is big or not. A triangle should be called
-- big if its hypoteneuse is longer than 10.
--    (be sure to use *let* to make your function
--     more readable)
bigTriangle :: Float -> Float -> Bool
bigTriangle length width =
  let hyp = sqrt (length ^ 2 + width ^ 2)
  in hyp > 10


-- Write a function that constructs a rect from a
-- list of 2 elements, length and width. Assume
-- there will always be exactly 2 elements
-- in the list. Use your *let* and pattern
-- matching skills to write the most readable
-- function you can.
listToRect :: [Integer] -> (Integer, Integer)
listToRect l =
  let [w, h] = l
  in (w, h)


-- Write a function that calculates the area of
-- each rectangle in a list. Write two versions
-- of it, one with with let *outside* the list,
-- and one with let *inside* the list.
areas :: Num a => [(a, a)] -> [a]
areas l = [ w * h | rect <- l, let (w, h) = rect]

areas' :: Num a => [(a, a)] -> [a]
areas' l =
  let area (w, h) = w * h
  in [area rect | rect <- l]

-- Write a function to calculate the area of a
-- square donut based on its size and thickness.
-- For instance, the square below has size 5,
-- thinkness 1, area 16.
--
--                   █████
--                   █   █
--                   █   █
--                   █   █
--                   █████
--
donutArea :: Num a => a -> a -> a
donutArea size thickness =
  let outer = size ^ 2
      inner = (size - 2 * thickness) ^ 2
  in outer - inner

