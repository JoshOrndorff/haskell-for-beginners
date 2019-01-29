-- Implement a program to make change. Your
-- program should accept a list of numbers
-- on stdin. The first number indicates the
-- amount of money to change, the rest
-- indicate the denominations of coin to
-- make change into.
--
-- The program should print out a list of
-- coins in the denominations given that totals
-- up to the amount of money being changed.
-- If there is money left over (i.e. the money
-- cannot be divided evenly into the denominations
-- given), the program should print the list of
-- coins and a message indicating how much
-- was left unchanged.
--
import System.Environment (getArgs)
import Control.Monad (when)
import Data.List (sort)

main = do
  -- Problem called for args at stdin, but what a pain.
  --input <- getContents
  input <- getArgs
  let (target : denoms') = map (read :: String -> Int) $ input
      denoms = (reverse . sort) denoms'
      (coins, change) = makeChange target denoms
  putStrLn ("Made change using " ++ show coins)
  case change of
    Nothing -> return ()
    Just c -> putStrLn $ (show c) ++ " units remaining"


makeChange :: Int -> [Int] -> ([Int], Maybe Int)
makeChange 0      _  = ([], Nothing)
makeChange target [] = ([], Just target)
makeChange target (d:enoms)
  | target < d = makeChange target enoms
  | otherwise =
    case makeChange (target - d) (d:enoms) of
      (tailCoins, Nothing) -> (d:tailCoins, Nothing)
      (tailCoins, Just change) ->
        case makeChange target enoms of
          (tailNoD, Nothing) -> (tailNoD, Nothing)
          (tailNoD, Just changeNoD) ->
            if change <= changeNoD
            then (d:tailCoins, Just change)
            else (d:tailNoD, Just changeNoD)
