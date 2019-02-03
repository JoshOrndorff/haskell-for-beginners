-- For all the problems here, try to write
-- the type signature first to understand
-- the problem. If you can't figure it out,
-- use GHCI to help you.
--

-- Use IO as a Functor to define an IO action
-- that reads a line from stdin as an Int
--
getIntLine :: IO Int
getIntLine = fmap read getLine


-- Define a function that will read an Int
-- from a String within any Functor, not
-- just IO. You are 'lifting' the read
-- function into the Functor.
--
readIntF :: Functor f => f String -> f Int
readIntF = fmap read


-- Define a new version of getIntLine that
-- uses readIntF
--
getIntLine' = readIntF getLine


-- Define a function that doubles an Int
-- read from a String, using Functions as
-- Functors.
--

-- WTF does "using functions as functors" mean?
-- read is certainly a funtion. Since I'm fmapping over it, that means its a functor too...
-- Ohh, this is a simple composition. We knew functions were functors.
readDoubleInt :: String -> Int
readDoubleInt = fmap (*2) read


-- Define a function that doubles an Int,
-- which is lifted any Functor.
--
doubleIntF :: Functor f => f Int -> f Int
doubleIntF = fmap (*2)


-- Define a new version of readDoubleInt
-- using doubleIntF
--
readDoubleInt' = doubleIntF read


-- Define an IO Action that reads a line
-- from stdin as an Int and doubles it,
-- using your functions above
--
getDoubleIntLine :: IO Int
getDoubleIntLine = fmap readDoubleInt getLine


-- Define a function that doubles an Int
-- read from a String lifted into any
-- Functor. Use functions from above to
-- make your definition as simple as possble.
--
readDoubleIntF :: Functor f => f String -> f Int
readDoubleIntF = fmap readDoubleInt


-- Define a new version of getDoubleIntLine
-- using readDoubleIntF
--
getDoubleIntLine' = readDoubleIntF getLine


-- My own additional exercise because I got tripped up on the graph one
data LinkedList a =
    Empty
  | Link a (LinkedList a)
    deriving (Show, Eq)

sampleList = Link 0 $ Link 1 $ Link 2 $ Empty

instance Functor LinkedList where
  fmap f Empty = Empty
  fmap f (Link x y) = Link (f x) ((fmap f) y)


-- Define a Functor instance for the Graph
-- type below. Demonstrate that it satisfies
-- the functor laws.
--
data Graph a = Node a [Graph a] deriving (Show, Eq)

sampleGraph = Node "Whiskey" [
    Node "Tango" [Node "Foxtrot" []],
    Node "Bang" []
  ]

instance Functor Graph where
  fmap f (Node x ys) = Node (f x) (fmap (fmap f) ys)


idProof = fmap id sampleGraph == id sampleGraph
compositionProof = fmap (length . reverse) sampleGraph == (fmap length $ fmap reverse sampleGraph)
