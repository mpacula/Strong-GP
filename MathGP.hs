{-
  Evaluates statements from the math grammar
-}

import Generator
import Types
import Genetic
import Possibly
import GrammarParser
import System.Random
import Data.List (sort)
import Debug.Trace (trace)
import qualified Data.Char as Ch

data MathValue = BinaryFunc (MathValue -> MathValue -> MathValue)
               | UnaryFunc  (MathValue -> MathValue)
               | Variable String
               | NumberLiteral Double

instance Show MathValue where
    show (BinaryFunc func)   = "binary_function"
    show (UnaryFunc func)    = "unary_function"
    show (Variable name)     = name
    show (NumberLiteral num) = show num


liftBinFunc :: (Double -> Double -> Double) -> (MathValue -> MathValue -> MathValue)
liftBinFunc func (NumberLiteral x) (NumberLiteral y) = NumberLiteral $ func x y

liftUnaryFunc :: (Double -> Double) -> (MathValue -> MathValue)
liftUnaryFunc func (NumberLiteral x) = NumberLiteral $ func x

isNumber :: String -> Bool
isNumber cs = (not . null) cs && all Ch.isDigit cs


isVariable :: String -> Bool
isVariable cs = length cs == 1 && (cs == "x" || cs == "y")


parseNumber :: String -> MathValue
parseNumber = NumberLiteral . read


parseVariable :: String -> MathValue
parseVariable = Variable


parseFunc :: String -> MathValue
parseFunc cs = case cs of
                 "+" -> BinaryFunc $ liftBinFunc (+)
                 "-" -> BinaryFunc $ liftBinFunc (-)
                 "*" -> BinaryFunc $ liftBinFunc (*)
                 "/" -> BinaryFunc $ liftBinFunc (/)
                 "sin" -> UnaryFunc $ liftUnaryFunc sin
                 "cos" -> UnaryFunc $ liftUnaryFunc cos
                 "tan" -> UnaryFunc $ liftUnaryFunc tan
                 _     -> error $ "Uknown function: " ++ cs


applyBinFunc :: [MathValue] -> MathValue
applyBinFunc values = let (BinaryFunc f) = values !! 1
                          arg1 = values !! 0
                          arg2 = values !! 2
                      in
                        f arg1 arg2


applyUnaryFunc :: [MathValue] -> MathValue
applyUnaryFunc values = let (UnaryFunc f) = values !! 0
                            arg = values !! 1
                        in
                          f arg
                        
                        


-- evaluates a single math statement
eval :: SyntaxTree -> MathValue
eval (Leaf val)
    | isNumber val   = parseNumber val
    | isVariable val = parseVariable val
    | otherwise      = parseFunc val
eval (Branch _ _ children)
     | length children == 3 = applyBinFunc tokens
     | length children == 2 = applyUnaryFunc tokens
     | length children == 1 = head $ map eval children
                              where tokens = map eval children


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

bestSelector :: GenerationMerger
bestSelector old new = take (length new) result
                       where
                         result = (reverse . sort) (old ++ new)

evoState = EvolverState { choices             = randoms (mkStdGen 42)             :: [Int]
                        , probabilities       = randomRs (0.0, 1.0) (mkStdGen 43) :: [Double]
                        , grammar             = possibly id (\_ -> []) expansions
                        , maxTreeDepth        = 100
                        , mutationProbability = 0.1
                        , evaluator           = Evaluator { runEval = (\tree ->
                                                                           do let (NumberLiteral num) = eval tree
                                                                              return $ 1.0 / (0.001 + (num - 1.141) ^ 2)
                                                                      )
                                                          }
                        , populationSize      = 100
                        , merger              = bestSelector
                        }

startTerm = (NonterminalTerm (PrimitiveType "Num"))

evoReporter :: EvolutionReporter
evoReporter trees = putStrLn $ "Mean fitness: " ++ (show . averageFitness) trees ++ ". Best: " ++ (show . fitness . bestMember) trees
