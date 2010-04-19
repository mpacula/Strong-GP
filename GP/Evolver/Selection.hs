module GP.Evolver.Selection
    (
      roulette
    , tournament
    ) where

import GP.Evolver.Subtree
import Control.Monad.State
import Data.List


type SelectionFunction = [EvaluatedSyntaxTree] -> EState a b c EvaluatedSyntaxTree

-- Picks an element t from a list of evaluated syntax trees with probability (fitness t).
-- All fitnesses have to be normalized.
roulette :: SelectionFunction
roulette trees
    = do pr <- randDouble
         let result = fst $ foldr picker ([], pr) trees
         if null result
         -- due to rounding errors, fitnesses might not add up to exactly 1
         -- In such cases, if pr is close to 1, no tree will be picked by the foldr
         -- so just assume the first one should have been picked (remember foldr
         -- goes right -> left).
         -- This happens *very* rarely, so this fix does no harm.
            then return $ head trees
            else return $ head result
    where
      picker t x@(picked, r)
          | null picked = if fitness t >= r
                          then ([t], r)
                          else ([], r - fitness t)
          | otherwise   = x


tournament :: SelectionFunction
tournament poolSize trees
    = do state <- get
         pool <- selectPool poolSize
         return $ (head . reverse . sort) pool
    where
      selectPool size = do if size == 0
                              then return []
                              else do elt <- randElt trees
                                      rest <- selectPool $ size-1
                                      return $ elt:rest

