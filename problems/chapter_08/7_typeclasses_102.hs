data Actor = DeNiro | Ferell | Stewart deriving (Eq, Show)

-- Define an instance of Ord for Actor that will
-- sort the Actors by how cool they are.
instance Ord Actor where
  compare DeNiro Ferell = LT
  compare DeNiro Stewart = LT
  compare Ferell Stewart = LT
  compare x y | x == y = EQ
  compare _ _ = GT

-- Using your Complex type from earlier, explicitly
-- define an instance of Eq for it
--
data Complex t = Complex t t deriving Show

instance Eq t => Eq (Complex t) where
  Complex ra ia == Complex rb ib = ra == rb && ia == ib

add :: Num t => Complex t -> Complex t -> Complex t
add (Complex ra ia) (Complex rb ib) = Complex (ra + rb) (ia + ib)

sub :: Num t => Complex t -> Complex t -> Complex t
sub (Complex ra ia) (Complex rb ib) = Complex (ra - rb) (ia - ib)

mult :: Num t => Complex t -> Complex t -> Complex t
mult (Complex ra ia) (Complex rb ib) = Complex (ra * rb - ia * ib) (ra * ib + rb * ia)


-- Define a Container typeclass that defines an interface
-- for unpacking a single item from any container type.
-- Define instances of Container for Maybe, [], Box, and Carton.
--
data Box t = B0
           | B1 t
           | B2 t t
           | B3 t t t
          deriving Show

data Carton t = Empty
              | Single t
              | Divider (Carton t) (Carton t)
              deriving Show

class Container c where
  unpack :: c a -> Maybe a

instance Container Maybe where
  unpack = id

instance Container [] where
  unpack [] = Nothing
  unpack (x:_) = Just x

instance Container Box where
  unpack B0 = Nothing
  unpack (B1 x) = Just x
  unpack (B2 x _) = Just x
  unpack (B3 x _ _) = Just x

instance Container Carton where
  unpack Empty = Nothing
  unpack (Single x) = Just x
  unpack (Divider l r) =
    case unpack l of
      j@(Just _) -> j
      _ -> unpack r
