{-
  Population generation and evolution
-}


module Genetic
    (
      startEvolving
    , EvolverState (..)
    , Evaluator (..)
    , EvolutionReporter
    , EvaluatedSyntaxTree (..)
    , GenerationMerger
    , StopCondition
    , bestMember
    , averageFitness
    ) where


import Generator (Term (..), SyntaxTree (..), Expansion, GeneratorState (..), startState, expand, expandTerms)
import Possibly (Possibly (..), possibly)
import Types (Type (..), isTypeCompatible)
import GrammarParser (parseGrammar)
import Utils (choose, indexFoldr)
import System.Random (mkStdGen, randoms, randomRs)
import Debug.Trace (trace)



type Population = [SyntaxTree]

type StopCondition = [EvaluatedSyntaxTree] -> Bool



data Subtree = Subtree { subtree :: SyntaxTree
                       , path    :: [Int]       -- path from root, each number represents a child index
                       }

instance Show Subtree where
    show sub = "\nPath: " ++ (show . path) sub ++ "\nSubtree:" ++ (show . subtree) sub


-- advances path to the subtree by one node
advancePath :: Subtree -> Subtree
advancePath subtree = subtree { path = (tail . path) subtree }



data EvolverState = EvolverState { choices             :: [Int]
                                 , probabilities       :: [Double]
                                 , grammar             :: [Expansion]
                                 , maxTreeDepth        :: Int
                                 , mutationProbability :: Double
                                 , evaluator           :: Evaluator
                                 , populationSize      :: Int
                                 , merger              :: GenerationMerger
                                 , stopCondition       :: StopCondition
                                 }

instance Show EvolverState where
    show state = "EvolverState"



-- transforms an evolver state into a generator state
mkGeneratorState :: EvolverState -> GeneratorState
mkGeneratorState evoState = startState (grammar evoState) (choices evoState) (maxTreeDepth evoState)


-- merges an evolver state with a generator state
mergeStates :: EvolverState -> GeneratorState -> EvolverState
mergeStates evoState genState = evoState { choices = stateChoices genState }


advance :: EvolverState -> EvolverState
advance state = state { choices = (tail . choices) state, probabilities = (tail . probabilities) state }
               

-- Expands the given term N times, generating N syntax trees
generatePopulation :: EvolverState -> Term -> Possibly (Population, EvolverState)
generatePopulation state startTerm = do
  (trees, finalGeneratorState) <- expandTerms (mkGeneratorState state) $ replicate (populationSize state) startTerm
  return (trees, mergeStates state finalGeneratorState)


-- Gets all subtrees of the given syntax tree, without leaves. The first element is guaranteed to be the root
-- tree
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
                                 else t { branchChildren = applyAtIndex
                                                           recurse
                                                           (head (path toReplace)) children
                                        }
        where
          recurse child
              | null (path toReplace) = replaceWith
              | otherwise             = replace child (advancePath toReplace) replaceWith



{-
  GENETIC OPERATORS - MUTATE & CROSSOVER
-}

-- returns the first list if not null, otherwise returns the second one
safeNull :: [a] -> [a] -> [a]
safeNull xs ys
    | null xs   = ys
    | otherwise = xs

-- performs a genetic crossover of 2 syntax trees. The returned tree is the first tree
-- with a random subtree replaced by a compatible one from the second tree
crossover :: EvolverState -> SyntaxTree -> SyntaxTree -> (SyntaxTree, EvolverState)
crossover state tree1 tree2 = let sub_tree1 = choose (choices state)           (subtrees tree1)
                                  sub_tree2 = choose (choices (advance state)) compatibles
                                  requiredType = ((branchType . subtree) sub_tree1)
                                  compatibles = safeNull (compatibleSubtrees requiredType (subtrees tree2))
                                                         (compatibleSubtrees requiredType (subtrees tree1))
                              in
                                ( replace tree1 sub_tree1 (subtree sub_tree2)
                                , advance $ advance state
                                )
    

-- mutates a random subtree with the probability given in the evolver state
mutate :: EvolverState -> SyntaxTree -> (SyntaxTree, EvolverState)
mutate state tree
       -- no mutation
       | (head . probabilities) state > (mutationProbability state) = (tree, state)
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
  FITNESS EVALUATION
-}

-- Evaluates a SyntaxTree and returns a fitness value. Since SyntaxTrees are programs,
-- evaluation can have arbitrary side effects and is therefore wrapped in the IO monad
newtype Evaluator = Evaluator {
         runEval :: SyntaxTree -> IO (Double)
      }


data EvaluatedSyntaxTree = EvaluatedSyntaxTree { fitness :: Double
                                               , tree    :: SyntaxTree
                                               } deriving (Show)

instance Eq EvaluatedSyntaxTree where
    a == b = (tree a) == (tree b)

instance Ord EvaluatedSyntaxTree where
    a `compare` b = (fitness a) `compare` (fitness b)


-- Evaluates fitness of a single tree
evalTree :: EvolverState -> SyntaxTree -> IO (EvaluatedSyntaxTree)
evalTree state tree = do fitnessVal <- ((runEval . evaluator) state) tree
                         return EvaluatedSyntaxTree { fitness = fitnessVal
                                                    , tree    = tree
                                                    }


-- Evaluates fitness of a population
evaluate :: EvolverState -> Population -> IO [EvaluatedSyntaxTree]
evaluate state population = do evaluated <- mapM (evalTree state) population
                               return evaluated


-- Normalizes fitness values of evaluated syntax trees so that all of them add up to 1
normalizeFitnesses :: [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree]
normalizeFitnesses xs = if totalFitness == 0
                        then map (\x -> x { fitness = 0 }) xs
                        else map (\x -> x { fitness = fitness x / totalFitness }) xs
    where
      totalFitness = foldr ((+) . fitness) 0 xs


-- Normalizes fitness values of evaluated syntax trees and picks an element t from them
-- with probability (fitness t). All fitnesses have to be normalized.
pickForReproduction :: EvolverState -> [EvaluatedSyntaxTree] -> SyntaxTree
pickForReproduction state trees = let rand = (head . probabilities) state
                                  in
                                    (head . fst) (foldr picker ([], rand) trees)
                                          where
                                            picker t x@(picked, r)
                                                   | null picked = if fitness t <= r
                                                                   then ([tree t], r)
                                                                   else ([], r - fitness t)
                                                   | otherwise   = x



-- Gets population member with the highest fitness value
bestMember :: [EvaluatedSyntaxTree] -> EvaluatedSyntaxTree
bestMember trees = foldr (\x currentMax ->
                              if fitness x > fitness currentMax
                              then x
                              else currentMax)
                   (head trees)
                   trees


-- Gets the average fitness of a population
averageFitness :: [EvaluatedSyntaxTree] -> Double
averageFitness trees
    | null trees = 0
    | otherwise = (foldr ((+) . fitness) 0 trees) / (fromIntegral $ length trees)


-- Function of this type gets syntax trees at each epoch right after evaluation
type EvolutionReporter = [EvaluatedSyntaxTree] -> IO ()


-- Evolves a new syntax tree from a population
evolveTree :: EvolverState -> [EvaluatedSyntaxTree] -> (SyntaxTree, EvolverState)
evolveTree initState trees = let parent1 = choose (choices initState) trees
                                 parent2 = choose ((choices . advance) initState) trees
                                 nextState = (advance . advance) initState
                                 (offspring, nextState') = crossover nextState (tree parent1) (tree parent2)
                                 (mutatedOffspring, finalState) = mutate nextState' offspring                               
                             in
                               (mutatedOffspring, finalState)


-- evolves a population of syntax trees
evolvePopulation :: EvolverState -> Int -> [EvaluatedSyntaxTree] -> ([SyntaxTree], EvolverState)
evolvePopulation initState member population
    | null population = ([], initState)
    | member == 0     = ([], initState)
    | otherwise       = let (evolvedTree,       nextState)  = evolveTree initState population
                            (otherEvolvedTrees, finalState) = evolvePopulation nextState (member - 1) population
                        in
                          ( evolvedTree : otherEvolvedTrees
                          , finalState
                          )
       
-- merges an old generation with the new one. Can be used to preserve best members from the old
-- generation etc
type GenerationMerger = [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree]

-- Evolves a population for a number of epochs
evolve :: EvolverState -> Int -> EvolutionReporter -> [EvaluatedSyntaxTree] -> IO ([EvaluatedSyntaxTree])
evolve initState epochs reporter population
    | epochs == 0    = return population
    | (stopCondition initState) population = return population
    | otherwise      = do let normalized                      = normalizeFitnesses population
                              (evolvedPopulation, finalState) = evolvePopulation initState (populationSize initState) normalized
                          evaluatedEvolvedPopulation <- evaluate finalState evolvedPopulation
                          reporter population
                          evolve finalState (epochs - 1) reporter ((merger finalState) population evaluatedEvolvedPopulation)


-- generates a new population and evolves it for a number of epochs
startEvolving :: EvolverState -> Term -> Int -> EvolutionReporter -> IO ([EvaluatedSyntaxTree])
startEvolving initState startTerm epochs reporter = do evaluated <- evaluate initState initialPopulation
                                                       evolve nextState epochs reporter evaluated
                                                    where
                                                      (initialPopulation, nextState) =
                                                          case generatePopulation initState startTerm of
                                                              Error msg ->
                                                                  error $ "Could not generate initial population: " ++ msg
                                                              Good x    -> x


{-
  DEBUG DEFINITIONS
-}

defaultReporter :: EvolutionReporter
defaultReporter trees = putStrLn "Evolving..."

expansions = parseGrammar $ unlines ["<Num> <BinOp> <Num> :: Num"
                                    , "+ :: BinOp"
                                    , "- :: BinOp"
                                    , "* :: BinOp"
                                    , "/ :: BinOp"
                                    , "<Func> <Num> :: Num"
                                    , "sin :: Func"
                                    , "cos :: Func"
                                    , "tan :: Func"
                                    , "1 :: Num"
                                    , "2 :: Num"
                                    , "3 :: Num"]



evoState = EvolverState { choices             = randoms (mkStdGen 42)             :: [Int]
                        , probabilities       = randomRs (0.0, 1.0) (mkStdGen 43) :: [Double]
                        , grammar             = possibly id (\_ -> []) expansions
                        , maxTreeDepth        = 100
                        , mutationProbability = 0.1
                        , evaluator           = Evaluator { runEval = (\t -> return 1)
                                                          }
                        , populationSize      = 100
                        , merger              = (\old new -> new)
                        , stopCondition       = (\_ -> False)
                        }


startTerm = (NonterminalTerm (PrimitiveType "Num"))


population = let p = generatePopulation evoState startTerm
             in
               case p of
                 Error msg       -> error msg
                 Good (trees, _) -> trees


tree1 = population !! 0
tree2 = population !! 1
tree3 = population !! 2

applyN :: (a -> a) -> Int -> a -> a
applyN f c arg
           | c <= 1   = f arg
           | otherwise = f $ applyN f (c - 1) arg


