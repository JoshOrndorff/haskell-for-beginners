-- Construct the word Gazump as a string in 4 different
-- ways and prove they are equal
s1 = "Gazump"
s2 = ['G', 'a', 'z', 'u', 'm', 'p']
s3 = 'G':'a':'z':'u':'m':'p':[]
s4 = ['G'] ++ ['a'] ++ ['z'] ++ ['u'] ++ ['m'] ++ ['p']

stringsEqual = s1 == s2 &&
               s2 == s3 &&
               s3 == s4

-- Write a function that totals top 3 numbers in a
-- list (assuming the list is sorted with highest first)
total l = sum (take 4 l)


-- Write a function to extract a portion of a string
-- based on position and length
substring start length s = take length (drop start s)

-- Write a function to tell if a list's length is > 4
-- (it should return a boolean)
longerThanFour l = length l > 4

-- Write a function like the one above *without* referring
-- to the list's length
longerThanFour' l = not (null (drop 4 l))

-- Write safe versions of tail and init that return
-- empty list if the list is empty
tail' l = if null l
          then []
          else tail l

init'  l = if null l
           then []
           else init l

-- Write safe versions of head and last that take a
-- default value to return if the list is empty
head' l def = if null l
              then def
              else head l
              
last' l def = if null l
              then def
              else last l

-- write a function to tell if either the sum or product
-- of a list is in another list
sumOrProductInOther l1 l2 = ((sum l1) `elem` l2) || ((product l1) `elem` l2)


-- write a function that reverses a section of a string
-- based on position and length. Use your substring function
-- from earlier to help.
partialReverse start length s = take start s ++
                                reverse (substring start length s) ++
                                drop (start + length) s

