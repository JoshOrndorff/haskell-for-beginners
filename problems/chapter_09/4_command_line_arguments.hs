-- Implement a program that processes input based on a command
-- specified as an argument on the command line. The mirror
-- command should reverse each line in the input. The reverseLines
-- command should reverse the order of lines in the input, without
-- changing the lines themselves.
--
-- If a second argument is given, it should be used as a filename
-- to read input from. Otherwise input should be read from stdin.
--
-- Btw, stdin :: Handle in System.IO ;)
--

import System.Environment
import System.IO

dispatchTable = [("mirror", mirror)
                ,("reverseLines", reverseLines)
                ]

mirror = unlines . map reverse . lines
reverseLines = unlines . reverse . lines


main = do
  args <- getArgs
  handle <- if length args == 2
            then openFile (args !! 1) ReadMode
            else return System.IO.stdin
  contents <- hGetContents handle
  let method = lookup (args !! 0) dispatchTable
  putStrLn $ case method of
               Just m -> m contents
               Nothing -> "Invalid Method specified"
