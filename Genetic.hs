{-
  Population generation and evolution
-}


module Genetic
    (
    ) where


import Generator
import Possibly
import Types
import GrammarParser (parseGrammar)
import Utils
import Debug.Trace (trace)
import System.Random


type Population = [SyntaxTree]


data Subtree = Subtree { subtree :: SyntaxTree
                       , path    :: [Int]       -- path from root, each number represents a child index
                       }


instance Show Subtree where
    show sub = "\nPath: " ++ (show . path) sub ++ "\nSubtree:" ++ (show . subtree) sub


advancePath :: Subtree -> Subtree
advancePath subtree = subtree { path = (tail . path) subtree }


data EvolverState = EvolverState { choices       :: [Int]
                                 , probabilities :: [Double]
                                 , grammar       :: [Expansion]
                                 , maxTreeDepth  :: Int
                                 }

instance Show EvolverState where
    show state = "EvolverState"

-- transforms an evolver state into a generator state
mkGeneratorState :: EvolverState -> GeneratorState
mkGeneratorState evoState = startState (grammar evoState) (choices evoState) (maxTreeDepth evoState)

-- merges an evolver state with a generator state
mergeStates :: EvolverState -> GeneratorState -> EvolverState
mergeStates evoState genState = evoState { choices = stateChoices genState }

--instance Monad (EvolverState a) where

advance :: EvolverState -> EvolverState
advance state = state { choices = (tail . choices) state, probabilities = (tail . probabilities) state }
               

-- Expands the given term N times, generating N syntax trees
generatePopulation :: EvolverState -> Term -> Int -> Possibly (Population, EvolverState)
generatePopulation state startTerm size = do
  (trees, finalGeneratorState) <- expandTerms (mkGeneratorState state) $ replicate size startTerm
  return (trees, mergeStates state finalGeneratorState)


-- Gets all subtrees of the given syntax tree, without leaves

subtrees :: SyntaxTree -> [Subtree]
subtrees = subtrees' []
    where
      subtrees' :: [Int] -> SyntaxTree -> [Subtree]
      subtrees' _    (Leaf _) = []
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


-- applies f only to the element at the given index, leaving the rest untouched
applyAtIndex :: (a -> a) -> Int -> [a] -> [a]
applyAtIndex f i = indexFoldr (\index elt sofar -> if index == i then (f elt) : sofar else elt : sofar) []


-- replaces a subtree in a syntax tree with a given syntax tree
replace :: SyntaxTree -> Subtree -> SyntaxTree -> SyntaxTree
replace root toReplace replaceWith =
    case root of
      t@(Leaf _)              -> error "Cannot replace a leaf"
      t@(Branch _ _ children) -> if null (path toReplace)
                                 then replaceWith
                                 else t { branchChildren = applyAtIndex recurse (head (path toReplace)) children }
        where
          recurse child
              | null (path toReplace) = replaceWith
              | otherwise             = replace child (advancePath toReplace) replaceWith


-- performs a genetic crossover of 2 syntax trees. The returned tree is the first tree
-- with a random subtree replaced by a compatible one from the second tree
crossover :: EvolverState -> SyntaxTree -> SyntaxTree -> (SyntaxTree, EvolverState)
crossover state tree1 tree2 = let sub_tree1 = choose (choices state) (subtrees tree1)
                                  sub_tree2 = choose (choices (advance state)) (subtrees tree2)
                              in
                                (replace tree1 sub_tree1 (subtree sub_tree2), advance $ advance state)


-- mutates a random subtree with the given probability
mutate :: EvolverState -> Double -> SyntaxTree -> (SyntaxTree, EvolverState)
mutate state probability tree
       -- no mutation
       | (head . probabilities) state > probability = (tree, state)
       -- mutate if a branch
       | otherwise = case subtree toMutate of
                       Leaf _                    -> (tree, state) 
                       Branch term reqdType _    ->
                           case expand genState (term { termRequiredType = reqdType }) of
                                                     Error msg -> trace ("Could not mutate: " ++ msg)
                                                                  (tree, state)
                                                     Good (newTree, nextGenState) ->
                                                         ( replace tree toMutate newTree
                                                         , mergeStates state nextGenState)
       where
         toMutate = choose (choices state) (subtrees tree)
         genState = mkGeneratorState state


{-
  DEBUG DEFINITIONS
-}

expansions = parseGrammar $ unlines ["<Num> <BinOp> <Num> :: Num"
                                    , "+ :: BinOp"
                                    , "- :: BinOp"
                                    , "* :: BinOp"
                                    , "/ :: BinOp"
                                    , "% :: BinOp"
                                    , "<Func> ( <Num> ) :: Num"
                                    , "sin :: Func"
                                    , "cos :: Func"
                                    , "tan :: Func"
                                    , "1 :: Num"
                                    , "2 :: Num"
                                    , "3 :: Num"]



evoState = EvolverState { choices       = randoms (mkStdGen 42)             :: [Int]
                        , probabilities = randomRs (0.0, 1.0) (mkStdGen 43) :: [Double]
                        , grammar       = possibly id (\_ -> []) expansions
                        , maxTreeDepth  = 10
                        }


population = let p = generatePopulation evoState (NonterminalTerm (PrimitiveType "Num")) 100
             in
               case p of
                 Error msg       -> error msg
                 Good (trees, _) -> trees


tree1 = population !! 0
tree2 = population !! 1
tree3 = population !! 2