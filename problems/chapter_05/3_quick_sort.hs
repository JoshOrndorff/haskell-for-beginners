-- Implement a bubble sort:
--   http://en.wikipedia.org/wiki/Bubble_sort
--
-- HINT: try using two functions, inOrder and bubbleSort

inOrder :: (Ord a) => [a] -> Bool
inOrder [] = True
inorder _:[] = True
inorder x:y:zs | x < y = inOrder y:zs
               | otherwise = False


bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort l | inOrder l = l
bubbleSort x:y:zs | x < y = y: bubbleSort x:zs
                  | otherwise x: bubbleSort y:zs
