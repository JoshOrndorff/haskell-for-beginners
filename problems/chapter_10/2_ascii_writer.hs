-- Implement a drawing machine that processes commands
-- given as single characters on stdin and then shows
-- the picture drawn by the commands. The commands
-- are:
--  l,r,u,d - move pen left,right,up,down
--  ^,v - pen up, pen down
--
-- All other characters can be ignored.
--
-- The machine should start at (0,0) with the pen up and
-- trace all points the machine touches while the pen is
-- down. Display all the points between (10,10) and (-10,-10)
-- as a picture to stdout.
--
-- Try this picture as a test:
--   llluuuuvldr^rrrrrrrrvurd^ddddv^dlvl^dlvllll^ulvl^ulv
--

import qualified Data.Set as Set

data PenState = Up | Down deriving (Show, Eq)
type Steps = String
type Drawing = Set.Set (Int, Int)

draw :: Steps -> (Int, Int) -> PenState -> Drawing -> Drawing
draw []         _   _      d = d
draw ('^':teps) point  _   d = draw teps point Up d
draw ('v':teps) point  pen d = draw teps point Down (Set.insert point d)
draw (s:teps)   (x, y) pen d =
  let
    (x', y') = case s of
      'u' -> (x+1, y)
      'd' -> (x-1, y)
      'l' -> (x, y-1)
      'r' -> (x, y+1)
      _   -> (x, y) -- Ignore invalid characters
    d' = if pen == Up then d else Set.insert (x', y') d
  in
    draw teps (x', y') pen d'


render :: Drawing -> IO ()
render d = do
  let rows = [-10..10]
  mapM_ (printRow d) rows

printRow :: Drawing -> Int -> IO ()
printRow d r = do
  let printPoints = [(r, c) | c <- [-10..10]]
  mapM_ (printPoint d) printPoints
  putChar '\n'

printPoint :: Drawing -> (Int, Int) -> IO ()
printPoint d p = if (Set.member p d ) then putChar '#' else putChar ' '


main = do
  steps <- getContents
  let drawing = draw steps (0, 0) Up Set.empty
  render drawing
