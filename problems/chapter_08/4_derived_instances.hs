import Data.List (maximumBy)
import Data.Function (on)

-- Augment your complex number data definition from section
-- 3 allow complex numbers to be equated, shown and read
--
data Complex t = Complex t t deriving (Eq, Show, Read)

-- Create a Player type that includes a name and a Score.
-- Then define a function to pick a winner from a list of
-- Players based on who has the highest score.

data Player = Player {name :: String, score :: Int} deriving (Eq, Ord, Show)

winner :: [Player] -> Player
winner = maximumBy (compare `on` score)
