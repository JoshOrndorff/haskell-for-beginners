-- import the Data.Map module and solve the following problem
--   (You may use other modules as well if you wish)
--
-- See the url below for the list of functions in Data.Map
--   http://www.haskell.org/ghc/docs/7.6-latest/html/libraries/containers-0.5.0.0/Data-Map-Lazy.html
--
import qualified Data.Map as Map

-- Define a function that will count the number of times
-- each character appears in a string
freqAnal :: String -> Map.Map Char Int
freqAnal "" = Map.empty
freqAnal (s:tring) = Map.insertWith (+) s 1 $ freqAnal tring

-- let's try it with a fold instead
freqAnal' :: String -> Map.Map Char Int
freqAnal' s = foldr update Map.empty s
 where update next acc = Map.insertWith (+) next 1 acc

-- Build a gene "database" from the following data that allows
-- the gene data to be looked up by gene name. Provide a function
-- that uses the db to lookup gene information.

geneData :: String
geneData =
  -- <Gene Name> <Ensembl Id> <Chromosome>
  "FKRP ENSG00000181027 Chromosome-19\n" ++
  "LMNA ENSG00000160789 Chromosome-1\n" ++
  "DYSF ENSG00000135636 Chromosome-2\n" ++
  "FKTN ENSG00000106692 Chromosome-9"

makeDB :: String -> Map.Map String String
makeDB raw =
  let
    lineToPair l =
      let ws = words l
      in (head ws, concat . tail $ ws)
    assocList = map lineToPair $ lines raw
  in
    Map.fromList assocList

geneLookup :: String -> Maybe String
geneLookup = flip Map.lookup $ makeDB geneData
