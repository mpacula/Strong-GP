module GP.Evolver.Subtree
    (
      Subtree(..)
    , advancePath
    , subtrees
    , compatibleSubtrees
    , replace
    ) where


import GP.Generator
import GP.Generator.Types

import GP.Utils


data Subtree = Subtree { subtree :: SyntaxTree
                       , path    :: [Int]       -- path from root, each number represents a child index
                       }

instance Show Subtree where
    show sub = "\nPath: " ++ (show . path) sub ++ "\nSubtree:" ++ (show . subtree) sub


-- advances path to the subtree by one node
advancePath :: Subtree -> Subtree
advancePath subtree = subtree { path = (tail . path) subtree }


-- Gets all subtrees of the given syntax tree, without leaves. The first element is guaranteed to be the root
-- tree
subtrees :: SyntaxTree -> [Subtree]
subtrees = subtrees' []
    where
      subtrees' :: [Int] -> SyntaxTree -> [Subtree]
      subtrees' _    l@(Leaf _) = []
      subtrees' path t@(Branch _ _ children) = (Subtree t path) : recurse children
          where
            recurse :: [SyntaxTree] -> [Subtree]
            recurse xs = indexFoldr
                         (\index elt subs ->
                              (subtrees' (path ++ [index]) elt) ++ subs)
                         []
                         xs


-- gets all subtrees compatible with the given type
compatibleSubtrees :: Type -> [Subtree] -> [Subtree]
compatibleSubtrees requiredType subs = filter (isCompatible . branchType . subtree) subs
                                       where
                                         isCompatible = (`isTypeCompatible` requiredType)



-- replaces a subtree in a syntax tree with a given syntax tree
replace :: SyntaxTree -> Subtree -> SyntaxTree -> SyntaxTree
replace root toReplace replaceWith =
    case root of
      t@(Leaf _)              -> error "Cannot replace a leaf"
      t@(Branch _ _ children) -> if null (path toReplace)
                                 then replaceWith
                                 else t { branchChildren = applyAtIndex
                                                           recurse
                                                           (head (path toReplace)) children
                                        }
        where
          recurse child
              | null (path toReplace) = replaceWith
              | otherwise             = replace child (advancePath toReplace) replaceWith
          

          -- applies f only to the element at the given index, leaving the rest untouched
          applyAtIndex :: (a -> a) -> Int -> [a] -> [a]
          applyAtIndex f i = indexFoldr
                             (\index elt sofar ->
                                  if index == i
                                  then (f elt) : sofar
                                  else elt : sofar)
                             []
