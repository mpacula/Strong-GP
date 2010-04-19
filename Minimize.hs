{--
 Copyright (c) 2010 Maciej Pacula

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

module Main (
             main
            ) where


import System.Random
import Data.Map
import Data.List

import GP.Evolver
import GP.Generator
import GP.Generator.Types
import GP.Generator.GrammarParser
import GP.Possibly
import GP.Utils

getGrammar :: Possibly [Production]
getGrammar = parseGrammar $ unlines [ "[x <- num] :: X"
                                    , "[y <- num] :: Y"
                                    , "<X, ch=crossover> <Y, ch=crossover> :: Args"]


extractNum :: SyntaxTree -> Double
extractNum (Branch _ _ [Leaf val1]) = read val1 :: Double


extractXY :: SyntaxTree -> (Double, Double)
extractXY atree = (extractNum xTree, extractNum yTree)
    where
      xTree = (branchChildren atree) !! 0
      yTree = (branchChildren atree) !! 1


numGenerator :: UserGenerator [Double]
numGenerator = UserGenerator {
                 generate = \rands _ -> (tail rands, show $ head rands)
               }



crossover = CrossoverHook { crossoverHook = \state tree1 tree2 -> 
                                             let num1 = extractNum tree1
                                                 num2 = extractNum tree2
                                                 avg = (num1 + num2) / 2
                                             in
                                               (state, tree1 { branchChildren = [Leaf $ show avg] } )
                          }


func :: Double -> Double -> Double
func x y = (x + 1456.89)^2 + (y - 5.43)^2


fitnessEvaluator :: state -> EvaluatedSyntaxTree -> (state, Double)
fitnessEvaluator state atree = (state, getFitness x y)
    where
      (x, y) = extractXY $ tree atree    
      getFitness :: Double -> Double -> Double
      getFitness x y = let val = func x y
                       in
                         1 / (1 + val)



reporter :: EvolutionReporter state
reporter generation state trees =
    do let bestTree = bestMember trees
           (bestX, bestY) = extractXY $ tree bestTree
       putStrLn $ "Generation " ++ show generation
                    ++ ": x=" ++ show bestX
                    ++ ", y=" ++ show bestY
                    ++ ", fitness=" ++ show (fitness bestTree)
       return state


elitistSelector :: GenerationMerger state
elitistSelector state oldPopulation newPopulation = (best ++ rest, state)
    where
      size = length oldPopulation
      ordered = reverse $ sort (oldPopulation ++ newPopulation)
      best = take (round ((fromIntegral size) / 100)) ordered
      rest = take (size - length best) newPopulation


runGA :: IO ()
runGA =
    do seed <- getStdRandom random :: IO (Int)
       let rs = randoms (mkStdGen seed) :: [Int]
           Good grammar = getGrammar
           params = Parameters { 
                                grammar = grammar
                              , maxGenerationDepth = 10
                              , mutationProbability = 0.2
                              , evaluator = makePureEvaluator fitnessEvaluator True
                              , populationSize = 1000
                              , merger = elitistSelector
                              , stopCondition = \_ -> False
                              , userGenerators = fromList [("num", (numGenerator, randomRs (-10000000.0, 10000000) (mkStdGen $ rs !! 2)))]
                              , completeMutationProbability = 0.1
                              , randomSelectionProbability = 0.1
                              , crossoverHooks = fromList [("crossover", (crossover, []))]
                              , select = tournament 20
                              , seed = rs !! 0
                              }
       runGP params [] (NonterminalTerm (PrimitiveType "Args") True "") 50 reporter
       return ()


main :: IO ()
main = runGA