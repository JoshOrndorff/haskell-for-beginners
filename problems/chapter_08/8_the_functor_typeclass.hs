import Data.Char (ord, chr)

-- Define functions to encrypt and decrypt character
-- data inside *any* Functor using Caesar cipher with
-- a shift 13.
--

encrypt :: Functor f => f Char -> f Char
encrypt = fmap $ chr . (+13) . ord

decrypt :: Functor f => f Char -> f Char
decrypt = fmap $ chr . subtract 13 . ord

-- Define instances for Functor for your Box and Carton
-- types. Once you've done that, try out using encrypt
-- on some Boxes and Cartons to protect their contents.
--
data Box t = B0
           | B1 t
           | B2 t t
           | B3 t t t
          deriving Show

instance Functor Box where
  fmap f B0 = B0
  fmap f (B1 x) = B1 $ f x
  fmap f (B2 x y) = B2 (f x) (f y)
  fmap f (B3 x y z) = B3 (f x) (f y) (f z)

data Carton t = Empty
              | Single t
              | Divider (Carton t) (Carton t)
              deriving Show


instance Functor Carton where
  fmap _ Empty = Empty
  fmap f (Single x) = Single $ f x
  fmap f (Divider l r) = Divider (fmap f l) (fmap f r)
