module HW0.T6 where

import Data.Char (isSpace)
import HW0.T1 (distrib)

a = distrib (Left ("AB" ++ "CD" ++ "EF"))

-- distrib (Left ("AB" ++ "CD" ++ "EF"))                            | (definition of distrib)
-- (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))       | (constructor (,) )
a_whnf = (Left ("AB" ++ "CD" ++ "EF"), Left ("AB" ++ "CD" ++ "EF"))

b = map isSpace "Hello, World"

-- map isSpace "Hello, World"                                       | (definition of map)
-- isSpace 'H' : map isSpace "ello, World"                          | (constructor (:) )
b_whnf = isSpace 'H' : map isSpace "ello, World"

c = if 1 > 0 || error "X" then "Y" else "Z"

-- if 1 > 0 || error "X" then "Y" else "Z"                          | (definition of if)
-- "Y"                                                              | (definition of "")
-- 'Y':[]                                                           | (constructor (:) )
c_whnf = 'Y' : []
