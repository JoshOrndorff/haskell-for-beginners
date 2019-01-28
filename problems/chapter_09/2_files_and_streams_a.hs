-- Note: GHCI disables buffering on stdin, which produces
-- unexpected (though correct) results for getContents and
-- friends. You can test your answers to the problems below
-- by changing main below to be whichever action you'd like
-- to run and using the runhaskell command at the shell prompt:
--
--   runhaskell <path to file>
--
main = chars

countYak [] = 0
countYak ('y':'a':'k':rest) = 1 + countYak rest
countYak (_:rest) = countYak rest

-- Define an action that uses getContents to read
-- lines from stdin and prints out the number of
-- yaks on each line. Verify that your action
-- prints out lines as they are typed in.
--
yakkityYak :: IO ()
yakkityYak = do
  contents <- getContents
  mapM_ (print . countYak) $ lines contents

-- Redefine the action above using interact
yak :: IO ()
yak = interact $ unlines . (map (show . countYak)) . lines


-- Define an action that reads lines from stdin
-- and after each line prints the total number of
-- characters read thus far.
--
chars = do
  contents <- getContents
  mapM_ print $ scanl1 (+) $ map length $ lines contents

-- BONUS: Try running these actions from GHCI and explain
-- what happens.

-- Still seems to work just fine for me not counting inability to end.
