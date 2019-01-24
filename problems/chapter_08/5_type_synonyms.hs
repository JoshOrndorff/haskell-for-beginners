-- Implement your complex number solution yet again, but
-- this time use a type synonym and represent complex
-- numbers using a tuple.
--
type Complex = (Float, Float)

add :: Complex -> Complex -> Complex
add (ra, ia) (rb, ib) = (ra + rb, ia + ib)

sub :: Complex -> Complex -> Complex
sub (ra, ia) (rb, ib) = (ra - rb, ia - ib)

mult :: Complex -> Complex -> Complex
mult (ra, ia) (rb, ib) = (ra * rb - ia * ib, ra * ib + rb * ia)

-- Don't remember the fractional typeclass, but the error message and answer key agree I need it here.
divide :: Complex -> Complex -> Complex
divide a (r, i) =
  let
    modulus = r^2 + i^2
    inverse = (r/modulus, i/modulus)
  in
    mult a inverse

-- Create a Result type based on Either that assumes the
-- left side is a String containing an error message. If you're
-- cool, partially apply the type constructor to get new
-- type.
--
type ErrorMsg = String
type Result = Either ErrorMsg


-- Define a version of Data.List.find that returns a Result.
-- It should return an error if the list is no matching element
-- is found, with appropriate messages in each case.
find' :: Eq a => (a -> Bool) -> [a] -> Result a
find' _ [] = Left "List was empty"
find' f (h:[]) | not (f h) = Left "Item not in list"
find' f (h:aystack) | f h = Right h
find' f (h:aystack) | not (f h) = find' f aystack
