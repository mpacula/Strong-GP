{-
  Population generation and evolution
-}


module GP.Genetic
    (
      startEvolving
    , EvolverState (..)
    , Evaluator (..)
    , EvolutionReporter
    , EvaluatedSyntaxTree (..)
    , GenerationMerger
    , StopCondition
    , CrossoverHook (..)
    , bestMember
    , averageFitness
    ) where


import GP.Generator (Term (..), SyntaxTree (..), Expansion, GeneratorState (..), startState, expand, expandFreeTerms,
                    UserGenerator)
import GP.Possibly (Possibly (..), possibly)
import GP.Types (Type (..), isTypeCompatible)
import GP.GrammarParser (parseGrammar)
import GP.Utils (choose, indexFoldr)
import System.Random (mkStdGen, randoms, randomRs)
import Data.Map (Map, fromList, (!), member, insert)
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



-- allows precise control over crossover
data CrossoverHook a = CrossoverHook { crossoverHook         :: a -> SyntaxTree -> SyntaxTree -> (a, SyntaxTree)
                                       
                                     }

hasHook :: EvolverState a b c -> String -> Bool
hasHook state name = name `member` (crossoverHooks state)

callCrossoverHook :: EvolverState a b c -> String -> SyntaxTree -> SyntaxTree -> (EvolverState a b c, SyntaxTree)
callCrossoverHook startState hookName tree1 tree2 = (startState, offspring)
    where
      (hook, hookState) = (crossoverHooks startState) ! hookName
      (newHookState, offspring) = (crossoverHook hook) hookState tree1 tree2

      newState = startState
                 {
                   crossoverHooks = insert hookName (hook, newHookState) (crossoverHooks startState)
                 }


data EvolverState a b c = EvolverState { choices             :: [Int]
                                       , probabilities       :: [Double]
                                       , grammar             :: [Expansion]
                                       , maxTreeDepth        :: Int
                                       , mutationProbability :: Double
                                       , evaluator           :: Evaluator a
                                       , populationSize      :: Int
                                       , merger              :: GenerationMerger
                                       , stopCondition       :: StopCondition
                                       , generationNumber    :: Int
                                       , userState           :: a
                                       , userGenerators      :: [(String, UserGenerator b, b)]
                                       , crossoverHooks      :: Map String (CrossoverHook c, c)
                                     }

instance Show (EvolverState a b c) where
    show state = "EvolverState"



-- transforms an evolver state into a generator state
mkGeneratorState :: EvolverState a b c -> GeneratorState b
mkGeneratorState evoState = startState (grammar evoState) (choices evoState) (maxTreeDepth evoState) (userGenerators evoState)


-- merges an evolver state with a generator state
mergeStates :: EvolverState a b c -> GeneratorState b -> EvolverState a b c
mergeStates evoState genState = evoState { choices = stateChoices genState }


-- advances all lists of random numbers by 1 element
advance :: EvolverState a b c -> EvolverState a b c
advance state = state { choices = (tail . choices) state, probabilities = (tail . probabilities) state }
               

-- increments population number
incGenerationNumber :: EvolverState a b c -> EvolverState a b c
incGenerationNumber state = state { generationNumber = (generationNumber state) + 1  }


-- Expands the given term N times, generating N syntax trees
generatePopulation :: EvolverState a b c -> Term -> Possibly (Population, EvolverState a b c)
generatePopulation state startTerm = do
  (trees, finalGeneratorState) <- expandFreeTerms (mkGeneratorState state) $ replicate (populationSize state) startTerm
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
crossover :: EvolverState a b c -> SyntaxTree -> SyntaxTree -> (SyntaxTree, EvolverState a b c)
crossover state tree1 tree2 = let sub_tree1 = choose (choices state)           eligibles
                                  hookName = crossoverHookName $ branchTerm $ subtree sub_tree1
                                  sub_tree2 = choose (choices (advance state)) compatibles
                                  requiredType = ((branchType . subtree) sub_tree1)
                                  compatibles = safeNull (compatibleSubtrees requiredType (subtrees tree2))
                                                         (compatibleSubtrees requiredType (subtrees tree1))
                                  eligibles = filter (not . locked . branchTerm . subtree) (subtrees tree1) 
                              in
                                if null hookName
                                then
                                    ( replace tree1 sub_tree1 (subtree sub_tree2)
                                    , advance $ advance state
                                    )
                                else if hasHook state hookName
                                     then
                                         let (finalState, offspring) = callCrossoverHook nextState hookName
                                                                       (subtree sub_tree1) (subtree sub_tree2)
                                             nextState = advance $ advance state
                                             in
                                               ( replace tree1 sub_tree1 offspring
                                               , finalState
                                               )
                                else
                                    error $ "Crossover hook not available: " ++ hookName
    

-- mutates a random subtree with the probability given in the evolver state
mutate :: EvolverState a b c -> SyntaxTree -> (SyntaxTree, EvolverState a b c)
mutate state tree
       -- no mutation
       | (head . probabilities) state > (mutationProbability state) = (tree, state)
       -- mutate if a branch
       | otherwise = case subtree toMutate of
                       Leaf _                    -> (tree, state) 
                       Branch term reqdType _    ->
                           case expand genState term { termRequiredType = reqdType } of
                                                     Error msg -> trace ("Could not mutate: " ++ msg)
                                                                  (tree, state)
                                                     Good (newTree, nextGenState) ->
                                                         ( replace tree toMutate newTree
                                                         , mergeStates state nextGenState)
       where
         toMutate = if head (probabilities state) > 0.5 then (subtrees tree) !! 0 else choose (choices state) (subtrees tree)
         genState = mkGeneratorState $ (advance . advance) state




{-
  FITNESS EVALUATION
-}

-- Evaluates a SyntaxTree and returns a fitness value. Since SyntaxTrees are programs,
-- evaluation can have arbitrary side effects and is therefore wrapped in the IO monad
newtype Evaluator a = Evaluator {
         runEval :: a -> SyntaxTree -> IO (a, Double)
      }


data EvaluatedSyntaxTree = EvaluatedSyntaxTree { fitness :: Double
                                               , tree    :: SyntaxTree
                                               } deriving (Show)

instance Eq EvaluatedSyntaxTree where
    a == b = (tree a) == (tree b)

instance Ord EvaluatedSyntaxTree where
    a `compare` b = (fitness a) `compare` (fitness b)


-- Evaluates fitness of a single tree
evalTree :: EvolverState a b c -> SyntaxTree -> IO (EvolverState a b c, EvaluatedSyntaxTree)
evalTree state tree = do (ustate, fitnessVal) <- ((runEval . evaluator) state) (userState state) tree
                         return (state {userState = ustate}
                                , EvaluatedSyntaxTree { fitness = fitnessVal
                                                      , tree    = tree
                                                      }
                                )


-- Evaluates fitness of a population
evaluate :: EvolverState a b c -> Population -> IO (EvolverState a b c, [EvaluatedSyntaxTree])
evaluate state [] = return (state, [])
evaluate state (p:ps) = do (nextState, evaluated) <- evalTree state p
                           (finalState, rest) <- evaluate nextState ps
                           return (finalState, evaluated : rest)


-- Normalizes fitness values of evaluated syntax trees so that all of them add up to 1
normalizeFitnesses :: [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree]
normalizeFitnesses xs = if totalFitness == 0
                        then map (\x -> x { fitness = 0 }) xs
                        else map (\x -> x { fitness = fitness x / totalFitness }) xs
    where
      totalFitness = foldr ((+) . fitness) 0 xs


-- Normalizes fitness values of evaluated syntax trees and picks an element t from them
-- with probability (fitness t). All fitnesses have to be normalized.
pickForReproduction :: EvolverState a b c -> [EvaluatedSyntaxTree] -> SyntaxTree
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
type EvolutionReporter a = Int -> a -> [EvaluatedSyntaxTree] -> IO (a)


-- Evolves a new syntax tree from a population
evolveTree :: EvolverState a b c -> [EvaluatedSyntaxTree] -> (SyntaxTree, EvolverState a b c)
evolveTree initState trees = let parent1                        = choose (choices initState) trees
                                 parent2                        = choose ((choices . advance) initState) trees
                                 nextState                      = (advance . advance) initState
                                 (offspring, nextState')        = crossover nextState (tree parent1) (tree parent2)
                                 (mutatedOffspring, finalState) = mutate nextState' offspring                               
                             in
                               (mutatedOffspring, finalState)


-- evolves a population of syntax trees
evolvePopulation :: EvolverState a b c -> Int -> [EvaluatedSyntaxTree] -> ([SyntaxTree], EvolverState a b c)
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
evolve :: EvolverState a b c -> Int -> EvolutionReporter a -> [EvaluatedSyntaxTree]
       -> IO (  (EvolverState a b c, [EvaluatedSyntaxTree])  )
evolve initState epochs reporter population
    | epochs == 0    = return (initState, population)
    | (stopCondition initState) population = do finalUserState <- reporter (generationNumber initState) (userState initState) population
                                                return (initState { userState = finalUserState }, population)
    | otherwise      = do let normalized                  = normalizeFitnesses population
                              (evolvedPopulation, state') = evolvePopulation initState (populationSize initState) normalized
                          (state'', reevaluatedOldPopulation) <- evaluate state' $ map tree population
                          (finalState, evaluatedEvolvedPopulation) <- evaluate state'' evolvedPopulation
                          finalUserState <- reporter (generationNumber finalState) (userState finalState) population
                          
                          evolve (incGenerationNumber finalState { userState = finalUserState })
                                 (epochs - 1) reporter
                                 ((merger finalState) reevaluatedOldPopulation evaluatedEvolvedPopulation)


-- generates a new population and evolves it for a number of epochs
startEvolving :: EvolverState a b c -> Term -> Int -> EvolutionReporter a -> IO ([EvaluatedSyntaxTree])
startEvolving initState startTerm epochs reporter = do (nextState2, evaluated) <- evaluate nextState initialPopulation
                                                       (_, finalPopulation) <- evolve nextState2 epochs reporter evaluated
                                                       return finalPopulation
                                                    where
                                                      (initialPopulation, nextState) =
                                                          case generatePopulation initState startTerm of
                                                              Error msg ->
                                                                  error $ "Could not generate initial population: " ++ msg
                                                              Good x    -> x


{-
  DEBUG DEFINITIONS
-}

defaultReporter :: EvolutionReporter Bool
defaultReporter generation state trees = do putStrLn $ show generation ++ ": Evolving..."
                                            return False

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
                        , evaluator           = Evaluator { runEval = (\s _ -> return (s, 1))
                                                          }
                        , populationSize      = 100
                        , merger              = (\old new -> new)
                        , stopCondition       = (\_ -> False)
                        , generationNumber    = 1
                        , userState           = False
                        , userGenerators      = []
                        , crossoverHooks      = fromList []
                        }


startTerm = (NonterminalTerm (PrimitiveType "Num") False "")


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


