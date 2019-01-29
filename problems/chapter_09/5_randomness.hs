import qualified Data.Map as Map
import System.Random

type Distribution = Map.Map Int Int

-- Define a function to simulate a roll of a
-- single 6 sided die by "rolling" it n times
-- and returning how many times each result appeared.
--
dieResults :: (RandomGen g) => g -> Int -> Distribution
dieResults g n = let
    samples = take n $ randomRs (1, 6) g
    assocList = map (flip (,) 1) samples
  in
    Map.fromListWith (+) assocList


-- Define a function to simulate a roll of a
-- n 6 sided dice once, adding the results
-- together, and returning a new random generator.
--
--

rollDice :: (RandomGen g) => g -> Int -> (Int, g)
rollDice g 0 = (0, g)
rollDice g n =
  let (thisRoll, nextGen) = randomR (1, 6) g
      (tailAns, gfinal) = rollDice nextGen (n - 1)
  in (thisRoll + tailAns, gfinal)

-- Define a function to simulate a any number
-- of rolls of n 6 sided by returning an infinite
-- list of roll results.
--
-- (use your rollDice function from above)
--
diceRolls :: (RandomGen g) => g -> Int -> [Int]
diceRolls g n =
  let (thisRoll, nextGen) = rollDice g n
  in thisRoll : (diceRolls nextGen n)

-- Define a function to simulate a roll of a
-- n 6 sided die by "rolling" it m times
-- and returning how many times each result appeared.
--
-- (use your diceRolls function from above)
--
diceResults :: (RandomGen g) => g -> Int -> Int -> Distribution
diceResults g n m = let
    samples = take m $ diceRolls g  n
    assocList = map (flip (,) 1) samples
  in
    Map.fromListWith (+) assocList

-- Define a function that accepts a number of dice
-- to roll and uses your diceResults function to build
-- an IO action that will simulate 1000 dice rolls and
-- print the results.
--
-- Make sure that the action runs a new simulation each
-- time it is run!
--
simulateDice :: Int -> IO ()
simulateDice n = do
  g <- getStdGen
  print $ diceResults g n 1000
