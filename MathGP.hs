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
import Data.Map (Map (..), fromList, (!), insert, member)
import Utils (fact)
import qualified Data.Char as Ch

-- represents all valid values within the language
data MathValue = BinaryFunc (MathValue -> MathValue -> MathValue)
               | UnaryFunc  (MathValue -> MathValue)
               | Variable String
               | NumberLiteral Double


instance Show MathValue where
    show (BinaryFunc func)   = "binary_function"
    show (UnaryFunc func)    = "unary_function"
    show (Variable name)     = name
    show (NumberLiteral num) = show num


-- state of expression evaluation
data EvalState = EvalState { variables :: Map String MathValue
                           }


-- creates initial evaluation state with no variable bindings
initState :: EvalState
initState = EvalState { variables = fromList []
                      }


-- binds a variable to a value in an evaluation state. If the variable is already bound, its value is
-- replaced
bindVariable :: String -> MathValue -> EvalState -> EvalState
bindVariable name val state = EvalState { variables = insert name val (variables state)
                                        }


-- checks if the state has a variable bound
isBound :: String -> EvalState -> Bool
isBound name = member name . variables


-- looks up a variable by name in an evaluation state and returns its value
lookupVariable :: String -> EvalState -> MathValue
lookupVariable name state = (variables state) ! name


liftBinFunc :: (Double -> Double -> Double) -> (MathValue -> MathValue -> MathValue)
liftBinFunc func (NumberLiteral x) (NumberLiteral y) = NumberLiteral $ func x y

liftUnaryFunc :: (Double -> Double) -> (MathValue -> MathValue)
liftUnaryFunc func (NumberLiteral x) = NumberLiteral $ func x

isNumber :: String -> Bool
isNumber cs = (not . null) cs && all Ch.isDigit cs


isVariable :: String -> Bool
isVariable cs = length cs == 1 && (cs == "x" || cs == "y")


evalNumber :: String -> MathValue
evalNumber = NumberLiteral . read


evalVariable :: EvalState -> String -> MathValue
evalVariable state name = if isBound name state
                          then lookupVariable name state
                          else error $ "Unbound variable: " ++ name


evalFunc :: String -> MathValue
evalFunc cs = case cs of
                 "+" -> BinaryFunc $ liftBinFunc (+)
                 "-" -> BinaryFunc $ liftBinFunc (-)
                 "*" -> BinaryFunc $ liftBinFunc (*)
                 "/" -> BinaryFunc $ liftBinFunc (\x y -> if y == 0 then 1000 else x / y)
                 "^" -> BinaryFunc $ liftBinFunc (\x y -> x ^^ (floor y))
                 "fact" -> UnaryFunc $ liftUnaryFunc (fromIntegral . fact . floor)
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
eval :: EvalState -> SyntaxTree -> MathValue
eval state (Leaf val)
    | isNumber val   = evalNumber val
    | isVariable val = evalVariable state val
    | otherwise      = evalFunc val
eval state (Branch _ _ children)
     | length children == 3 = applyBinFunc tokens
     | length children == 2 = applyUnaryFunc tokens
     | length children == 1 = head tokens
                              where tokens = map (eval state) children


-- evaluates an exression as a function of one variable x
evalAsFunc :: SyntaxTree -> Double -> Double
evalAsFunc tree arg = let result = eval (bindVariable "x" (NumberLiteral arg) initState) tree
                      in
                        case result of
                          (NumberLiteral val) -> val
                          other               -> error $ "Tree did not evaluate to a number, but to: " ++ show other


-- computes total | square error between a function prototype and a syntax tree
-- Minimum error: 0, corresponds to perfect match
regressionError :: (Double -> Double) -> (Double, Double) -> Int -> SyntaxTree -> Double
regressionError f (rmin, rmax) samples tree = foldr (+) 0 errors
                                              where
                                                step = (rmax - rmin) / (fromIntegral samples)
                                                samplingPts = [rmin, rmin + step .. rmax]
                                                errors = map (\pt -> ((f pt) - (evalAsFunc tree pt)) ^ 2) samplingPts


expansions = parseGrammar $ unlines ["<Num> <BinOp> <Num> :: Num"
                                    , "+ :: BinOp"
                                    , "- :: BinOp"
                                    , "* :: BinOp"
                                    , "/ :: BinOp"
                                    , "^ :: BinOp"
--                                    , "<Func> <Num> :: Num"
--                                    , "fact :: Func"
--                                    , "sin :: Func"
--                                    , "cos :: Func"
--                                    , "tan :: Func"
                                    , "0 :: Num"
                                    , "1 :: Num"
                                    , "2 :: Num"
                                    , "3 :: Num"
                                    , "4 :: Num"
                                    , "5 :: Num"
                                    , "6 :: Num"
                                    , "7 :: Num"
                                    , "8 :: Num"
                                    , "9 :: Num"
                                    , "x :: Num"]

testedFunction :: Double -> Double
testedFunction = (\x -> sin x)


bestSelector :: GenerationMerger
bestSelector old new = select result ++ select new
                       where
                         result = (reverse . sort) (old ++ new)
                         size = length new
                         select = take (floor ((fromIntegral size) / 2))


evoState = EvolverState { choices             = randoms (mkStdGen 42)             :: [Int]
                        , probabilities       = randomRs (0.0, 1.0) (mkStdGen 43) :: [Double]
                        , grammar             = possibly id (\_ -> []) expansions
                        , maxTreeDepth        = 100
                        , mutationProbability = 0.5
                        , evaluator           =
                            Evaluator { runEval = (\tree ->
                                                       let error = regressionError testedFunction (-1, 1) 100 tree
                                                       in
                                                         if isNaN error
                                                         then return 0
                                                         else return $ 1.0 / (0.001 + error)
                                                  )
                                      }
                        , populationSize      = 100
                        , merger              = bestSelector
                        , stopCondition       = \trees -> (fitness . bestMember $ trees) >= 1000.0
                        }

defaultState = bindVariable "x" (NumberLiteral 3.14) initState

startTerm = (NonterminalTerm (PrimitiveType "Num"))

evoReporter :: EvolutionReporter
evoReporter trees = putStrLn $ "Mean fitness: " ++ (show . averageFitness) trees ++ ". Best: " ++ (show . fitness . bestMember) trees

evalBest :: [EvaluatedSyntaxTree] -> Double -> Double
evalBest population x = (((flip evalAsFunc) x) . tree) $ bestMember population
