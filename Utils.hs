{--
  Various utility functions that are used commonly yet are not part of
  a discernable bigger abstraction.
--}

module Utils
    (
     choose
   , xor
   , partitionEithers
   , indexFoldr
    ) where


import Data.List (partition)

-- given a list of indices, takes the first one and returns an element of a list at that
-- index. The index is wrapped around to make sure it doesn't go out of bounds.
-- 
-- WARNING: the list has to be finite
choose :: [Int] -> [a] -> a
choose choices xs = xs !! ((`mod` size) . head) choices
                    where
                      size = length xs


-- binary exclusive or
xor :: Bool -> Bool -> Bool
xor x y = x /= y


-- returns all Lefts
lefts :: [Either a b] -> [Either a b]
lefts = filter (either (\_ -> True) (\_ -> False))


-- returns all Rights
rights :: [Either a b] -> [Either a b]
rights = filter (either (\_ -> False) (\_ -> True))


-- splits eithers into a pair of Lefts and Rights, respectively
partitionEithers :: [Either a b] -> ([Either a b], [Either a b])
partitionEithers = partition (either (\_ -> True) (\_ -> False))


-- like foldr, but passes the index of iteration to the folding function, where the first element
-- in the list has index 0.
indexFoldr :: (Int -> a -> b -> b) -> b -> [a] -> b
indexFoldr f initial xs = snd $ foldr folder (length xs - 1, initial) xs
                          where
                            folder = (\x (index, sofar) -> (index - 1, f index x sofar))

