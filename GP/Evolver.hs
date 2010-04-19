{--

 Copyright (c) 2009 Maciej Pacula

 Permission is hereby granted, free of charge, to any person
 obtaining a copy of this software and associated documentation
 files (the "Software"), to deal in the Software without
 restriction, including without limitation the rights to use,
 copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the
 Software is furnished to do so, subject to the following
 conditions:

 The above copyright notice and this permission notice shall be
 included in all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 OTHER DEALINGS IN THE SOFTWARE.

--}

{-
  Population generation and evolution
-}


module GP.Evolver
    (
      runGP
    , makePureEvaluator
    , makeImpureEvaluator
    , EvolutionReporter
    , EvaluatedSyntaxTree (..)
    , GenerationMerger
    , StopCondition
    , CrossoverHook (..)
    , Parameters (..)
    , bestMember
    , averageFitness
    , roulette
    , tournament
    ) where



import GP.Possibly (Possibly (..), possibly)

import GP.Generator
import GP.Generator.Types (Type (..), isTypeCompatible)
import GP.Generator.GrammarParser (parseGrammar)

import GP.Evolver.Subtree

import GP.Utils (indexFoldr)

import System.Random (mkStdGen, randoms, randomRs)
import Control.Monad.State
import Control.Parallel
import Control.Parallel.Strategies

import Data.Map (Map, fromList, (!), member, insert)
import Data.List (sort)



{----------------------------------------------------------------------
  Type definitions
----------------------------------------------------------------------}

-- user-visible parameters that control the GP algorithm
-- a: user state type
-- b: user generator state type
-- c: crossover hook state type
data Parameters a b c = Parameters {
      {- Tree generation -}
      grammar                       :: [Production]      
    , maxGenerationDepth            :: Int

      {- Probabilities -}
    , mutationProbability           :: Double
    , completeMutationProbability   :: Double
    , randomSelectionProbability    :: Double

      {- General -}
    , seed                          :: Int
    , populationSize                :: Int
    , stopCondition                 :: StopCondition      

      {- Genetic Operators -}
    , select                        :: SelectionFunction a b c
    , evaluator                     :: Evaluator a
    , merger                        :: GenerationMerger a
    , userGenerators                :: Map String (UserGenerator b, b)
    , crossoverHooks                :: Map String (CrossoverHook c, c)
    }


-- internal state used by the GP algorithm
data InternalState a b c = InternalState {
      {- Random numbers -}

      -- [MIN_INT, MAX_INT]
      randInts                      :: [Int]
                                       
      -- [0.0, 1.0]
    , randDoubles                   :: [Double]

      {- General algorithm state -}
    , parameters                    :: Parameters a b c
    , generationNumber              :: Int
    , userState                     :: a
    }


-- shorthand for an instance of the State type that uses the GP's
-- custom State type
type EState a b c d  = State (InternalState a b c) d


-- selects a population member for reproduction
type SelectionFunction a b c = [EvaluatedSyntaxTree] -> EState a b c EvaluatedSyntaxTree


-- a shorthand for a function that takes a list of evaluated syntax trees
-- from the current generation and returns True iff evolution should stop
type StopCondition = [EvaluatedSyntaxTree] -> Bool


-- allows precise control over crossover. CrossoverHooks can be defined per-type, and are called
-- to crossover two subtrees with the same root type.
-- Each hook takes some uder-defined state and two parent subtrees, and should return a new state
-- and an offspring subtree. It is the responsibility of the implementer to ensure that the offspring
-- is type-compatible within its context.
data CrossoverHook a = CrossoverHook { crossoverHook :: a -> SyntaxTree -> SyntaxTree -> (a, SyntaxTree)
                                     }

hasHook :: InternalState a b c -> String -> Bool
hasHook state name = name `member` (crossoverHooks $ parameters state)

callCrossoverHook :: String -> SyntaxTree -> SyntaxTree -> EState a b c SyntaxTree
callCrossoverHook hookName tree1 tree2 = do state <- get                                          
                                            let params = parameters state
                                                (hook, hookState) = (crossoverHooks params) ! hookName
                                                (newHookState, offspring) = (crossoverHook hook) hookState tree1 tree2
                                            put state {
                                                      parameters = params {
                                                                     crossoverHooks = insert hookName
                                                                                      (hook, newHookState)
                                                                                      (crossoverHooks params)
                                                                   }
                                                    }
                                            return offspring


-- transforms an evolver state into a generator state. No evolver state is modified
makeGeneratorState :: EState a b c (GeneratorState b)
makeGeneratorState = do state <- get
                        let params = parameters state
                        return $ startState
                                   (grammar params)
                                   (randInts state)
                                   (maxGenerationDepth params)
                                   (userGenerators $ parameters state)


-- merges an evolver state with a generator state by copying the generator's random numbers
-- to the evolver's state 
fromGeneratorState :: GeneratorState b -> EState a b c ()
fromGeneratorState genState = do evoState <- get
                                 put evoState {
                                           randInts = stateChoices genState
                                         }


-- gets a random integer from the evolver's internal state
randInt :: EState a b c Int
randInt = do state <- get
             let r = head $ randInts state
             put state { randInts = tail $ randInts state }
             return r

-- gets a random double 0-1 from the evolver's internal state
randDouble :: EState a b c Double
randDouble = do state <- get
                let r = head $ randDoubles state
                put state { randDoubles = tail $ randDoubles state }
                return r

-- chooses a random element from a list
randElt :: [a] -> EState b c d a
randElt [] = error "Tried to get a random element from an empty list"
randElt lst = do n <- randInt
                 let index = n `mod` (length lst)
                 return $ lst !! index



-- probabilistic amb: returns alt1 with probability pr, and alt2 with 
-- probability 1-pr
pamb :: Double -> EState a b c d -> EState a b c d -> EState a b c d
pamb pr alt1 alt2 = do random <- randDouble
                       if random < pr
                         then alt1
                         else alt2


-- increments population number
incGenerationNumber :: EState a b c ()
incGenerationNumber = do state <- get
                         put state { generationNumber = (generationNumber state) + 1  }
                         return ()


-- Expands the given term N times, generating N syntax trees
generatePopulation :: Term -> EState a b c (Possibly [SyntaxTree])
generatePopulation startTerm = do genState <- makeGeneratorState
                                  evoState <- get
                                  let n = populationSize $ parameters evoState
                                      genResult = expandFreeTerms genState $ replicate n startTerm
                                  case genResult of
                                       Error msg -> return $ Error ("Could not generate population:\n\t" ++ msg)
                                       Good (trees, genState') -> do fromGeneratorState genState'
                                                                     return $ Good trees
                                                                         



{-
  GENETIC OPERATORS - MUTATE & CROSSOVER
-}

-- returns the first list if not null, otherwise returns the second one
ifNotNullElse :: [a] -> [a] -> [a]
ifNotNullElse xs ys
    | null xs   = ys
    | otherwise = xs


mergeHistories :: [Double] -> [Double] -> [Double]
mergeHistories [] y = y
mergeHistories x [] = x
mergeHistories (x:xs) (y:ys) = (x + y)/2:mergeHistories xs ys

-- performs a genetic crossover of 2 syntax trees. The returned tree is the first tree
-- with a random subtree replaced by a compatible one from the second tree
crossover :: EvaluatedSyntaxTree -> EvaluatedSyntaxTree -> EState a b c EvaluatedSyntaxTree
crossover tree1 tree2 = do let notLocked = filter (not . locked . branchTerm . subtree) (subtrees $ tree tree1)
                           sub_tree1 <- randElt notLocked
                                        
                           let hookName = crossoverHookName $ branchTerm $ subtree sub_tree1
                               requiredType = ((branchType . subtree) sub_tree1)
                               compatibles = ifNotNullElse (compatibleSubtrees requiredType (subtrees $ tree tree2))
                                             (compatibleSubtrees requiredType (subtrees $ tree tree1))

                           if (not.null) compatibles
                             then
                               do sub_tree2 <- randElt compatibles
                                  state <- get
   
                                  if null hookName
                                    then
                                        return EvaluatedSyntaxTree {
                                                     tree = replace (tree tree1) sub_tree1 (subtree sub_tree2)
                                                   , fitness = -1
                                                   , fitnessHistory = mergeHistories (fitnessHistory tree1) (fitnessHistory tree2)
                                                   }
                                    else if hasHook state hookName
                                         then
                                             do offspring <- callCrossoverHook hookName (subtree sub_tree1) (subtree sub_tree2)
                                                return EvaluatedSyntaxTree {
                                                             tree = replace (tree tree1) sub_tree1 offspring
                                                           , fitness = -1
                                                           , fitnessHistory = mergeHistories (fitnessHistory tree1) (fitnessHistory tree2)
                                                           }
                                         else
                                             error $ "Crossover hook not available: " ++ hookName
                             else
                               return tree1
    

-- mutates a random subtree with the probability given in the evolver state
mutate :: EvaluatedSyntaxTree -> EState a b c EvaluatedSyntaxTree
mutate atree = do state <- get
                  let params = parameters state
                  toMutate <- pamb (completeMutationProbability params)
                              (return $ (subtrees (tree atree)) !! 0)
                              (randElt $ subtrees (tree atree))

                  let regenerated = (subtree toMutate) == (tree atree)

                  genState <- makeGeneratorState
                  case subtree toMutate of
                    Leaf _ -> return atree
                    Branch term reqdType _ ->
                        case expand genState term { termRequiredType = reqdType } of
                          Error msg                 -> error $"Mutation failed: " ++ msg
                          Good (genTree, genState') -> do fromGeneratorState genState'
                                                          return atree { tree = replace (tree atree) toMutate genTree
                                                                       , fitnessHistory = if regenerated
                                                                                          then []
                                                                                          else fitnessHistory atree
                                                                       }

{-
  FITNESS EVALUATION
-}

-- Evaluates a SyntaxTree and returns a fitness value. Since SyntaxTrees are programs,
-- evaluation can have arbitrary side effects and is therefore wrapped in the IO monad
data Evaluator a = Evaluator {
      runImpureEval :: a -> EvaluatedSyntaxTree -> IO (a, Double)
    , runPureEval :: a -> EvaluatedSyntaxTree -> (a, Double)
    , pure :: Bool
    , stateless :: Bool
    }


makePureEvaluator :: (a -> EvaluatedSyntaxTree -> (a, Double)) -> Bool -> Evaluator a
makePureEvaluator f stateless = Evaluator {
                                  runImpureEval = (\_ _ -> error "This evaluator is pure and cannot be run as impure")
                                , runPureEval = f
                                , pure = True
                                , stateless = stateless
                        }


makeImpureEvaluator :: (a -> EvaluatedSyntaxTree -> IO (a, Double)) -> Evaluator a
makeImpureEvaluator f = Evaluator {
                          runImpureEval = f
                        , runPureEval = (\_ _ -> error "This evaluator is impure and cannot be run as pure")
                        , pure = False
                        , stateless = False
                      }


data EvaluatedSyntaxTree = EvaluatedSyntaxTree { fitness :: Double
                                               
                                               -- head -> tail: most recent -> least recent
                                               , fitnessHistory :: [Double]

                                               , tree    :: SyntaxTree
                                               } deriving (Show)

instance Eq EvaluatedSyntaxTree where
    a == b = (tree a) == (tree b)

instance Ord EvaluatedSyntaxTree where
    a `compare` b = (fitness a) `compare` (fitness b)



-- Evaluates fitness of a population

evaluate :: [EvaluatedSyntaxTree] -> InternalState a b c -> IO ([EvaluatedSyntaxTree], InternalState a b c)
evaluate trees state
         | pure (evaluator $ parameters state) = evaluatePure trees state
         | otherwise = evaluateImpure trees state

evaluateImpure [] s = return ([], s)
evaluateImpure (p:ps) s = do (ep, s') <- evalTree p s
                             (eps, s'') <- evaluate ps s'
                             return (ep:eps, s'')
    where
      -- Evaluates fitness of a single tree
      evalTree :: EvaluatedSyntaxTree -> InternalState a b c -> IO (EvaluatedSyntaxTree, InternalState a b c)
      evalTree atree state = do (ustate, fitnessVal) <- ((runImpureEval . evaluator . parameters) state) (userState state) atree
                                return (atree { fitness = fitnessVal
                                              , fitnessHistory = fitnessVal : fitnessHistory atree
                                              }
                                       , state { userState = ustate }
                                       )


evaluatePure :: [EvaluatedSyntaxTree] -> InternalState a b c -> IO ([EvaluatedSyntaxTree], InternalState a b c)
evaluatePure trees state
    | stateless (evaluator $ parameters state) =
        let eval = runPureEval (evaluator $ parameters state) (userState state)
            evaluatedTrees = parMap rwhnf (\tree ->
                                               let (_, fitnessVal) = eval tree
                                               in
                                                 tree { fitness = fitnessVal
                                                      , fitnessHistory = fitnessVal : fitnessHistory tree
                                                      }
                                          )
                             trees
        in
          return (evaluatedTrees, state)

    | otherwise = undefined



-- normalizes fitness values of evaluated syntax trees so that they add up to 1
normalizeFitnesses :: [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree]
normalizeFitnesses xs = if totalFitness == 0
                        then map (\x -> x { fitness = 0 }) xs
                        else map (\x -> x { fitness = fitness x / totalFitness }) xs
    where
      totalFitness = foldr ((+) . fitness) 0 xs
                                               

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
averageFitness [] = error "No trees to compute average fitness of"
averageFitness trees
    | null trees = 0
    | otherwise = (foldr ((+) . fitness) 0 trees) / (fromIntegral $ length trees)


-- Function of this type gets syntax trees at each epoch right after evaluation
type EvolutionReporter a b c = Int -> a -> [EvaluatedSyntaxTree] -> Parameters a b c -> IO (a, Parameters a b c)


-- Evolves a new syntax tree from a population
evolveTree :: [EvaluatedSyntaxTree] -> EState a b c EvaluatedSyntaxTree
evolveTree trees = do state <- get
                      let params = parameters state
                      r1 <- randDouble
                      r2 <- randDouble
                      parent1          <- if r1 >= randomSelectionProbability params
                                          then select params trees
                                          else randElt trees
                      parent2          <- if r2 >= randomSelectionProbability params
                                          then select params trees
                                          else randElt trees
                      offspring        <- crossover parent1 parent2
                      mutatedOffspring <- pamb (mutationProbability params) (mutate offspring) (return offspring)
                      return mutatedOffspring


-- evolves a population of syntax trees
evolvePopulation :: Int -> [EvaluatedSyntaxTree] -> EState a b c [EvaluatedSyntaxTree]
evolvePopulation member population
    | null population = return []
    | member == 0     = return []
    | otherwise       = do evolvedTree <- evolveTree population
                           otherEvolvedTrees <- evolvePopulation (member - 1) population
                           return $ evolvedTree : otherEvolvedTrees

       
-- merges an old generation with the new one. Can be used to preserve best members from the old
-- generation etc. The type parameter "a" represents user state
type GenerationMerger a = a -> [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree] -> ([EvaluatedSyntaxTree], a)

callGenerationMerger :: [EvaluatedSyntaxTree] -> [EvaluatedSyntaxTree] -> EState a b c [EvaluatedSyntaxTree]
callGenerationMerger old new = do state <- get
                                  let ustate = userState state
                                      m = merger $ parameters state
                                      (merged, ustate') = m ustate old new
                                  put state { userState = ustate' }
                                  return merged


-- evolves a population for a number of epochs.
-- Note: we want this to live inside the IO monad, so we can't use State monad's conveniences
-- (or maybe we can but my haskell vodoo isn't strong enough)
evolve :: InternalState a b c -> Int -> EvolutionReporter a b c -> [EvaluatedSyntaxTree]
       -> IO (  (InternalState a b c, [EvaluatedSyntaxTree])  )
evolve initState epochs reporter population
    | epochs == 1    = return (initState, population)
    | (stopCondition $ parameters initState) population =
        do (finalUserState, finalParameters) <- reporter (generationNumber initState) (userState initState) population (parameters initState)
           return (initState { userState = finalUserState, parameters = finalParameters }, population)
                                                       
    | otherwise      = do let (evolvedPopulation, evoState) = runState evolveHelper initState
                          (reevaluatedOldPopulation, evoState') <- evaluate population evoState
                          (evaluatedEvolvedPopulation, evoState'') <- evaluate evolvedPopulation evoState'
                          (newUserState, newParameters) <- reporter (generationNumber evoState'') (userState evoState'') evaluatedEvolvedPopulation (parameters evoState'')
                          let evoState''' = execState incGenerationNumber evoState'' { userState = newUserState, parameters = newParameters }
                              genMerger = callGenerationMerger reevaluatedOldPopulation evaluatedEvolvedPopulation
                              (nextPopulation, finalEvoState)  = runState genMerger evoState'''
                          evolve finalEvoState (epochs - 1) reporter nextPopulation
    where
      test = []
      evolveHelper :: EState a b c [EvaluatedSyntaxTree]
      evolveHelper = do let normalized = normalizeFitnesses population
                        state <- get
                        evolvedPopulation <- evolvePopulation (populationSize $ parameters state) normalized
                        return evolvedPopulation
                            


-- generates a new population and evolves it for a number of epochs
startEvolving :: InternalState a b c -> Term -> Int -> EvolutionReporter a b c -> IO ([EvaluatedSyntaxTree])
startEvolving initState startTerm epochs reporter = do (evaluated, nextState2) <- evaluate initialEvaluatedPopulation nextState
                                                       (newUserState, newParameters) <- reporter (generationNumber nextState2)
                                                                                        (userState nextState2)
                                                                                        evaluated
                                                                                        (parameters nextState2)
                                                       (_, finalPopulation) <- evolve nextState2 { userState = newUserState, parameters = newParameters, generationNumber = 1 + generationNumber nextState2 } epochs reporter evaluated
                                                       return finalPopulation
                                                    where
                                                      (initialPopulation, nextState) =
                                                          case runState (generatePopulation startTerm) initState of
                                                              (Error msg, _) ->
                                                                  error $ "Could not generate initial population: " ++ msg
                                                              (Good x, s)    -> (x, s)
                                                      initialEvaluatedPopulation = map (\t -> EvaluatedSyntaxTree {  fitness = -1, fitnessHistory = [], tree = t }) initialPopulation


runGP :: Parameters a b c -> a -> Term -> Int -> EvolutionReporter a b c -> IO ([EvaluatedSyntaxTree])
runGP params userState = startEvolving startState
    where
      seeds = randoms (mkStdGen $ seed params) :: [Int]

      startState = InternalState {
                     randInts = randoms (mkStdGen $ seeds !! 0)
                   , randDoubles = randomRs (0.0, 1.0) (mkStdGen $ seeds !! 1) :: [Double]
                   , parameters = params
                   , generationNumber = 1
                   , userState = userState
                   }



{----------------------------------------------------------------------
  Selection functions

  Responsible for selecting a population member for reproduction.


  Two built-in selection functions are provided:

    - roulette: does roulette wheel (fitness-proportional) selection
    - tournament: does tournament selection of the given pool size
----------------------------------------------------------------------}

-- Picks an element t from a list of evaluated syntax trees with probability (fitness t).
-- All fitnesses have to be normalized.
roulette :: [EvaluatedSyntaxTree] -> EState a b c EvaluatedSyntaxTree
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


tournament :: Int -> [EvaluatedSyntaxTree] -> EState a b c EvaluatedSyntaxTree
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
