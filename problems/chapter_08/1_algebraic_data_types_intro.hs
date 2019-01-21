-- Define a data type that represents a Complex number:
--   http://en.wikipedia.org/wiki/Complex_number
--   Use Floats simplicity
--
data Complex = Complex Float Float deriving Show

-- Define add, sub, mult, and divide for Complex numbers

add :: Complex -> Complex -> Complex
add (Complex ra ia) (Complex rb ib) = Complex (ra + rb) (ia + ib)

sub :: Complex -> Complex -> Complex
sub (Complex ra ia) (Complex rb ib) = Complex (ra - rb) (ia - ib)

mult :: Complex -> Complex -> Complex
mult (Complex ra ia) (Complex rb ib) = Complex (ra * rb - ia * ib) (ra * ib + rb * ia)

divide :: Complex -> Complex -> Complex
divide a (Complex r i) =
  let
    modulus = r^2 + i^2
    inverse = Complex (r/modulus) (i/modulus)
  in
    mult a inverse

-- Define a set of data types allowing sandwiches to be constructed.
-- A sandwich include a choice of bread (bun or roll), patty (beef,
-- chicke, or veggie) and any numbers of toppings (ketchup, mustard,
-- cheese, lettuce, and tomato).
--
-- When you're done, you should be able to construct a sandwich like this:
--   plain Roll Veggie `with` Cheese `with` Tomato `with` Ketchup
--
data Sandwich = Sandwich Bread Patty [Topping] deriving Show
data Bread = Bun | Roll deriving Show
data Patty = Beef | Chicken | Veggie deriving Show
data Topping = Ketchup | Mustard | Cheese | Lettuce | Tomato deriving Show

plain :: Bread -> Patty -> Sandwich
plain b p = Sandwich b p []

with :: Sandwich -> Topping -> Sandwich
with (Sandwich b p ts) t = Sandwich b p (t:ts)



-- Define a function to calculate the damage done (as an int)
-- by a fighter in a fight, at a certain range to their target.
--
--   * A fighter has some amount of strength and is armed in some way.
--   * A sword has a damage value.
--   * A bow has a range value and a damage value.

--   * A fighter armed only fists deals their strength in damage.
--   * A fighter armed with a sword deals their strength + the
--     the damage value for the sword.
--   * A fighter armed with a bow deals the bow's damage value.
--   * A fighter armed with fists or a sword does no damage beyond range 0.
--   * A fighter armed with a bow deals no damage beyond the bow's range.
--
data Fighter = Fighter Int Weapon
data Weapon = Fists
            | Sword Int    -- strength
            | Bow Int Int  -- strength range

calculateDamage :: Fighter -> Int -> Int
calculateDamage (Fighter strength Fists         ) 0 = strength
calculateDamage (Fighter strength (Sword damage)) 0 = strength + damage
calculateDamage (Fighter _ (Bow strength r1)) r2 | r2 < r1 = strength
calculateDamage _ _ = 0
