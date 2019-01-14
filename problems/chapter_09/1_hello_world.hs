-- Define an action below that will print a hello world message.
-- Execute the program in GHCI.

import Control.Monad (when)

helloWorld :: IO ()
helloWorld = putStrLn "Hello world"

-- Add a type declaration for helloWorld.

-- Write, compile and run a hello world program from scratch
-- in another file.
--
-- see answers/chapter_09/1_hello_world/helloworld.hs

-- Define a function that takes a secret string and returns
-- an action that will ask the user to guess the secret.
-- Tell the user whether they guessed the secret correctly.
-- Be sure to include a type declaration.
--

guessSecret :: String -> IO ()
guessSecret  secret = do
  putStr "Guess the password: "
  guess <- getLine
  putStrLn (if guess == secret then "You guessed correctly" else "Better luck next time")

-- Define an IO action that purports to generate random
-- numbers, but always produces 9.
--
getRandom = print 9 -- Guess they were looking for return 9


-- Define a funtion that prompts the user with a given string
-- and reads the user's response. It should return the user's
-- response, but ensure there is only one space between words
-- (and none on the beginning or end). Include a type declaration!
--
prompt :: String -> IO String
prompt p = do
  putStrLn p
  response <- getLine
  return $ fixWhiteSpace response
    where fixWhiteSpace = unwords . words -- stole this bit from the answer

-- Define an action that will ask the user 3 questions before they
-- may pass over the bridge. The first two questions always ask for
-- their name and quest. Normally the last question asks their
-- favorite color, but knights named Robin are asked what the
-- capital of Assyria is, and kings name Arthur are queried about the
-- speed of unbaggaged airborn fowl.
--
-- Judge the answers however you see fit, and tell the user whether
-- they are allowed to cross the bridge or are cast into the Gorge
-- of Eternal Peril. Use the prompt function you just wrote, and
-- write any more functions for yourself to use as you see fit.
--
bridge :: IO ()
bridge = do
  name <- prompt "What is your name"
  prompt "What is your quest"
  let q3 = getq3 name
  prompt q3
  putStrLn (if name == "Joshy" then "You may cross" else "Gorge of Eternal Peril")

getq3 :: String -> String
getq3 "Robin" = "What is the capital of Assyria?"
getq3 "Arthur" = "What is the speed of an unladen swallow?"
getq3 _ = "What is your favorite color?"

-- Define an action which will prompt the user three times
-- for a number. Then print out the sum, product, maximum, and
-- minimum of the numbers the user entered. Use some of the IO
-- helpers discussed in the text to make your life easier.
--
stats :: IO ()
stats = do
  answers <- mapM prompt (replicate 3 "Tell me a number")
  let nums = map read answers
  putStrLn ""
  print $ sum nums
  print $ maximum nums
  print $ product nums
  print $ minimum nums
