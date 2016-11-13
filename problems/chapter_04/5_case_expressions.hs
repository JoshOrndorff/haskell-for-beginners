-- Rewrite your implementation of reverse from earlier
-- using a case expression.
reverse' :: [a] -> [a]
reverse' items = case items of [] -> []
                               (x:xs) -> (reverse' xs) ++ [x]

-- Write a function to greet people based on their names.
-- The function should:
--    * Give some response for all names.
--    * Have a special greating for Bob
--    * Insult anyone who's name starts with Q
--    * Tell anyone with a name shorter than
--      3 characters to get a real name
--
-- BONUS: This is more convenient if you use a guard
--        *inside* your case experesion.

greeting :: String -> String
greeting name = case name of "Bob" -> "I used to be named that too."
                             'Q':rest -> "That's a stupid name."
                             other
                               | length other <= 3 -> "Get a real name."
                               | otherwise -> "Hello " ++ name

