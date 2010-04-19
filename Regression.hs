module Main
    (
     main
    ) where


import GP.Evolver
import GP.Generator
import GP.Generator.Types
import GP.Generator.GrammarParser

import GP.Possibly

import Data.List
import qualified Data.Map as M


import System.Random

distanceList :: (Double -> Double) -> (Double -> Double) -> [Double] -> [Double]
distanceList f1 f2 args = norm
    where
      distance (x1, x2) = f1 x1 - f2 x2
      argPairs = [(x1, x2) | x1 <- args, x2 <- args]

      nnorm = map distance argPairs
      sumVal = sum $ map abs nnorm

      norm = map (/sumVal) nnorm


distance :: (Double -> Double) -> (Double -> Double) -> [Double] -> Double
distance f1 f2 args = sum $ map (\(d1,d2) -> (d1-d2)*(d1-d2)) (zip ds1 ds2)
    where
      ds1 = distanceList f1 f2 args
      ds2 = distanceList f1 f1 args


getGrammar :: Possibly [Production]
getGrammar = parseGrammar $ unlines [ "[val <- num]:: Number"
                                    , "x :: Expr"
                                    , "<Number, ch=crossover> :: Expr"
                                    , "<Expr> <Op> <Expr> :: OpExpr"
                                    , "<OpExpr> :: Expr"
                                    , "+ :: Op"
                                    , "- :: Op"
                                    , "* :: Op"
 --                                   , "/ :: Op"
                                    , "<Func> <Expr> :: Call"
                                    , "<Call> :: Expr"
                                    , "sin :: Func"
                                    , "cos :: Func"
                                    , "tan :: Func"
--                                    , "<Number, ch=crossover> <Op> <OpExpr> :: OpExpr"
                                    ]


data Op = Call String (Double -> Double)
        | Lookup String
        | Number Double
        | BinOp String (Double -> Double -> Double)

instance Show Op where
    show (Call s f) = "Call \"" ++ s ++ "\"" ++ " (" ++ s ++ ")"
    show (Lookup s) = "Lookup \"" ++ s ++ "\""
    show (Number n) = "Number " ++ show n
    show (BinOp op _) = "BinOp \"" ++ op ++ "\" " ++ "(" ++ op ++ ")"


exec :: (String -> Double) -> [Double] -> [Op] -> [Double]
exec lookup stack [] = stack
exec lookup stack (Number n : rest) = exec lookup (n : stack) rest
exec lookup stack (Call _ func : rest) = exec lookup ((func (head stack)):(tail stack)) rest
exec lookup stack (BinOp _ op : rest) = exec lookup ( ((stack !! 0) `op` (stack !! 1)):(drop 2 stack) ) rest
exec lookup stack (Lookup name : rest) = exec lookup (lookup name : stack) rest



compile :: SyntaxTree -> [Op]

compile (Leaf "+") = [BinOp "+" (+)]
compile (Leaf "-") = [BinOp "-" (-)]
compile (Leaf "*") = [BinOp "*" (*)]
--compile (Leaf "/") = [BinOp "/" (/)]

compile (Leaf "sin") = [Call "sin" (sin)]
compile (Leaf "cos") = [Call "cos" (cos)]
compile (Leaf "tan") = [Call "tan" (tan)]

compile (Leaf "x") = [Lookup "x"]

compile (Leaf n) = [Number $ read n]

compile (Branch _ (PrimitiveType "OpExpr") (lhs:op:rhs:[])) = compile rhs ++ compile lhs ++ compile op
compile (Branch _ (PrimitiveType "Call") (func:arg:[])) = compile arg ++ compile func
compile (Branch _ (PrimitiveType "Number") [expr]) = compile expr
compile (Branch _ (PrimitiveType "Func") [expr]) = compile expr
compile (Branch _ (PrimitiveType "Expr") [expr]) = compile expr
compile (Branch _ (PrimitiveType "Op") [expr]) = compile expr


compile other = error $ "Unsupported syntax tree: " ++ show other

                                 

extractNum :: SyntaxTree -> Double
extractNum (Branch _ _ [Leaf val1]) = read val1 :: Double

crossover = CrossoverHook { crossoverHook = \state tree1 tree2 -> 
                                             let num1 = extractNum tree1
                                                 num2 = extractNum tree2
                                                 avg = (num1 + num2) / 2
                                             in
                                               (state, tree1 { branchChildren = [Leaf $ show avg] } )
                          }

numGenerator :: UserGenerator [Double]
numGenerator = UserGenerator {
                 generate = \rands _ -> (tail rands, show $ abs (head rands))
               }


totalError :: Double -> Double -> Double -> (Double -> Double) -> (Double -> Double) -> Double
totalError min step max f1 f2 = sum diff
    where
      args = [min, min+step .. max]
      diff = map (\x -> abs ((f1 x) - (f2 x)))
             args


testFunc :: Double -> Double
testFunc x = x*x/20

               

fitnessEvaluator :: state -> EvaluatedSyntaxTree -> (state, Double)
fitnessEvaluator state atree = let compiled = compile $ tree atree
                                   asFunc = (\x -> head $ exec (\ "x" -> x) [] compiled)
                                   err1 = totalError (-1.0) 0.01 1.0 testFunc asFunc
                                   err2 = distance testFunc asFunc [-1.0, -1.0 + 0.1 .. 1.0]
                                   err = err1
                               in
                                 (state, 1 / (0.01 + err + 1*(fromIntegral $ length compiled)))


reporter :: EvolutionReporter Double b c
reporter generation prevFitness trees params =
    do let bestTree = bestMember trees
           compiled = compile $ tree bestTree
           asFunc = (\x -> head $ exec (\ "x" -> x) [] compiled)
           fitnessDelta = abs $ fitness bestTree - prevFitness
           oldMutationRate = mutationProbability params
           newMutationRate = if fitnessDelta / prevFitness > 0.2
                             then max 0.1 (oldMutationRate - 0.2)
                             else min 0.7 (oldMutationRate + 0.0125)

       putStrLn $ "Generation " ++ show generation
                    ++ ", fitness=" ++ show (fitness bestTree)
                    ++ ", total error=" ++ show (totalError (-1.0) 0.01 1.0 testFunc asFunc)
                    ++ ", mutation rate=" ++ show oldMutationRate

       putStrLn $ flattenTree (tree bestTree)
       putStrLn $ show (compile (tree bestTree))
       putStrLn "\n"
       return (fitness bestTree, params {mutationProbability = newMutationRate})


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
                              , maxGenerationDepth = 20
                              , mutationProbability = 0.1
                              , evaluator = makePureEvaluator fitnessEvaluator True
                              , populationSize = 2000
                              , merger = elitistSelector
                              , stopCondition = \_ -> False
                              , userGenerators = M.fromList [("num", (numGenerator, randomRs (-100.0, 100) (mkStdGen $ rs !! 2)))]
                              , completeMutationProbability = 0.1
                              , randomSelectionProbability = 0.1
                              , crossoverHooks = M.fromList [("crossover", (crossover, []))]
                              , select = tournament 20
                              , seed = rs !! 0
                              }
       runGP params 0 (NonterminalTerm (PrimitiveType "Expr") False "") 500 reporter
       return ()


main :: IO ()
main = runGA
