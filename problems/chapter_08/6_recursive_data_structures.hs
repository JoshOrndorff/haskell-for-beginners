-- Define a Carton type that is similar to your
-- Box type from earlier, but which is recursive.
--
-- Instead of being able to contain two items, it
-- should be able to contain a divider with a Carton
-- on each side.
data Carton t = Empty
              | Single t
              | Divider (Carton t) (Carton t)
              deriving Show


-- Define a version of map for Cartons.
map' :: (a -> b) -> Carton a -> Carton b
map' _ Empty = Empty
map' f (Single x) = Single $ f x
map' f (Divider l r) = Divider (map' f l) (map' f r)

-- tests
a = Divider (Divider (Single 3) (Single 4)) (Divider (Single 5) Empty)
b = map' (*2) a

-- Define a version of filter for Cartons
filter' :: (a -> Bool) -> Carton a -> Carton a
filter' _ Empty = Empty
filter' f s@(Single x) | f x = s
                       | otherwise = Empty
filter' f (Divider l r) = Divider (filter' f l) (filter' f r)


-- Define a version of find for Cartons
find' :: (a -> Bool) -> Carton a -> Maybe a
find' _ Empty = Nothing
find' f (Single x) | f x = Just x
                   | otherwise = Nothing
find' f (Divider l r) =
  case find' f l of
    j@(Just _) -> j
    Nothing -> find' f r
