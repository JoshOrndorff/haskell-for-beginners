-- Define a function that accepts a filename
-- and returns an action that will read the file
-- and print out each line reversed. Use `openFile`
-- to accomplish this.
--
-- You can use "samples/mirror.txt" to test your action.

import System.IO

main = reverseEachLine "../../samples/mirror.txt"

helper :: String -> String
helper = unlines . map reverse . lines

reverseEachLine :: FilePath -> IO ()
reverseEachLine filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  putStrLn $ helper contents

-- Define another function like the one above, but
-- use `withFile`
--
reverseEachLine' :: FilePath -> IO ()
reverseEachLine' filename = withFile filename ReadMode $ (\h -> do
  contents <- hGetContents h
  putStrLn $ helper contents)


-- Define yet another function like the one above, but
-- use `readFile`
--
reverseEachLine'' :: FilePath -> IO ()
reverseEachLine'' filename = do
  contents <- readFile filename
  putStrLn $ helper contents

-- Define a function that accepts *2* filenames and
-- returns an action that will read the contents of the
-- first file, reverse each line, and write it to the second
-- file
--
reverseFile :: FilePath -> FilePath -> IO ()
reverseFile inFile outFile = do
  input <- readFile inFile
  writeFile outFile $ helper input
