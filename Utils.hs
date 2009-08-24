{--
  Various utility functions that are used commonly yet are not part of
  a discernable bigger abstraction.
--}

module Utils
    (
     choose
    ) where

-- given a list of indices, takes the first one and returns an element of a list at that
-- index. The index is wrapped around to make sure it doesn't go out of bounds.
-- 
-- WARNING: the list has to be finite
choose :: [Int] -> [a] -> a
choose choices xs = xs !! ((`mod` size) . head) choices
                    where
                      size = length xs

