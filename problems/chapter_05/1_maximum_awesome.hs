-- Define a list of all Fibonacci numbers

fib :: Num a => [a]
fib = [f n | n <- [1..]]
  where f 0 = 0
        f 1 = 1
        f n = f (n - 1) + f (n - 2)
-- I definitely would not have gotten this on my own, but it makes sense now.
-- I think I could have written f on my own had I been asked, and then
-- written fib given f though. That where binding is sexy.





-- Implement a recursive function to find
-- the maximum element in a list of
-- pairs of elements.

maxPairElement :: Ord a => [(a,a)] -> a
maxPairElement [(x, y)] = max x y
maxPairElement ((x, y):pairs)
  | headMax > otherMax = headMax
  | otherwise = otherMax
    where headMax = max x y 
          otherMax = maxPairElement pairs
