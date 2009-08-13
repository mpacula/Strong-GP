{--
 Provides simple matching on types without directly inspecting contents 
--}

module AbstractTypeMatching
    (
     Matcher
   , (==>)
    ) where

type Matcher a b = a -> Maybe b

-- Matcher combinator. Given two matchers, creates a new matcher which calls
-- the first one and then the second one only if the result of the first is Nothing
(==>) :: Matcher a b -> Matcher a b -> Matcher a b
firstMatcher ==> secondMatcher = chainedMatcher where
    chainedMatcher arg =
        case fstResult of
          Nothing -> secondMatcher arg
          x@(Just _) -> fstResult
        where fstResult = firstMatcher arg
