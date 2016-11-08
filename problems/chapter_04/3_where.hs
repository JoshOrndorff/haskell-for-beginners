-- Write a function to tell whether a triangle
-- is big or not. A triangle should be called
-- big if its hypoteneuse is longer than 10.
--    (be sure to use where to make your function
--     more readable)
bigTriangle :: Float -> Float -> Bool
bigTriangle length width
  | hyp <= 10 = False
  | otherwise = True
    where hyp = sqrt (length * length + width * width)

-- Write a function that classifies rectangles
-- into at least 5 size categories based their
-- area. The pair given is the rectangle's length
-- and width.
rectSize :: (Integer, Integer) -> String
rectSize (width, height)
  | a < 5 = "tiny"
  | a < 10 = "small"
  | a < 20 = "average"
  | a < 40 = "large"
  | otherwise = "huge"
    where a = width * height

-- Write a function that constructs a rect from a
-- list of 2 elements, length and width. Assume
-- there will always be exactly 2 elements
-- in the list. Use your where and pattern
-- matching skills to write the most readable
-- function you can.
listToRect :: [Integer] -> (Integer, Integer)
listToRect l = (w, h)
                 where [w, h] = l

-- Write a function that calculates the area of
-- each rectangle in a list.
rectAreas :: Num a => [(a, a)] -> [a]
rectAreas rs = [area r | r <- rs]
                 where area (l, w) = l * w
