solveRPN ::  String -> Float
solveRPN = head . foldl folder [] . words
  where folder (a:b:cs) "+" = (a+b):cs
        folder (a:b:cs) "-" = (a-b):cs
        folder (a:b:cs) "*" = (a*b):cs
        folder (a:b:cs) "/" = (a/b):cs
        folder (a:b:cs) "^" = (a**b):cs
        folder (a:bs) "ln"  = (log a):bs
        folder xs "sum"   = (sum xs):[]
        folder stack  x   = (read x) : stack
