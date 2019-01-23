-- Re-implement your complex number solution from section
-- 1 to work with complex numbers for any numeric (Num) type.
--
data Complex t = Complex t t deriving Show

add :: Num t => Complex t -> Complex t -> Complex t
add (Complex ra ia) (Complex rb ib) = Complex (ra + rb) (ia + ib)

sub :: Num t => Complex t -> Complex t -> Complex t
sub (Complex ra ia) (Complex rb ib) = Complex (ra - rb) (ia - ib)

mult :: Num t => Complex t -> Complex t -> Complex t
mult (Complex ra ia) (Complex rb ib) = Complex (ra * rb - ia * ib) (ra * ib + rb * ia)

-- Don't remember the fractional typeclass, but the error message and answer key agree I need it here.
divide :: Fractional t => Complex t -> Complex t -> Complex t
divide a (Complex r i) =
  let
    modulus = r^2 + i^2
    inverse = Complex (r/modulus) (i/modulus)
  in
    mult a inverse

-- Define a Box type that can hold 0, 1 or 2 items of
-- another type. Define a boxMap function analogous to
-- map on lists for the box type.
--
data Box t = B0
           | B1 t
           | B2 t t
           | B3 t t t
          deriving Show

boxMap :: (a -> b) -> Box a -> Box b
boxMap _  B0        = B0
boxMap f (B1 x)     = B1 $ f x
boxMap f (B2 x y)   = B2 (f x) (f y)
boxMap f (B3 x y z) = B3 (f x) (f y) (f z)

-- Define a Crate type that can be empty, hold one thing
-- of a type, one thing of another type, or one thing of each
-- of those types. (i.e. it's like a Box, but when holding two
-- items they don't have to be the same type).
--
-- Define a Crate function that flips order of the *types*
-- in the crate. I.E. The type of a Crate holding an Int and
-- a String would change to the type of a Crate holding a String
-- and an Int, even if the Crate was only holding one item.
--

data Crate a b = Empty
               | First a
               | Second b
               | Both a b
               deriving Show

flipCrate :: Crate a b -> Crate b a
flipCrate  Empty      = Empty
flipCrate (First x  ) = Second   x
flipCrate (Second  y) = First  y
flipCrate (Both  x y) = Both   y x
