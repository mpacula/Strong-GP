module Main
    (
     main
    ) where


import GP.Evolver
import GP.Generator
import GP.Generator.Types
import GP.Generator.GrammarParser

import GP.Possibly
import GP.Utils (chooseArray)

import Data.Array.IArray
import Data.List
import qualified Data.Map as M

import System.Random
import System (getArgs)

import Debug.Trace (trace)


getGrammar :: Possibly [Production]
getGrammar = parseGrammar $ unlines [
              {--
                Numerical
              --}

              -- number primitives
              "[val <- num] :: Number"
             , "[val <- fraction] :: Fraction"
             , "r :: Var"
             , "g :: Var"
             , "b :: Var"
--             , "h :: Var"
--             , "s :: Var"
--             , "v :: Var"
             , "<Var> :: NumExpr"
             , "<Number, ch=crossover> :: NumExpr"
             , "<Fraction, ch=crossover> :: NumExpr"

             -- infix number expressions
             , "+ :: NumOp"
             , "- :: NumOp"
             , "* :: NumOp"
             , "/ :: NumOp"
             , "<Var> / <Var> :: Ratio"
             , "<NumOpExpr> / <Var> :: Ratio"
             , "<NumExpr> <NumOp> <NumExpr> :: NumOpExpr"
             , "<Var> - <Var> :: NumOpExpr"
             , "<NumOpExpr> :: NumExpr"

             -- number function calls
             , "<NumFunc> <NumExpr> :: NumCall"
--             , "<NumCall> :: NumExpr"


             {--
               Boolean
             --}

             -- boolean infix expressions
             , "AND :: BoolOp"
             , "OR :: BoolOp"
             , "NOT :: BoolUnaryOp"
             , "<BoolUnaryOp> <EqExpr> :: BoolUnaryOpExpr"
             , "<EqExpr> <BoolOp> <BoolBinaryOpExpr> :: BoolBinaryOpExpr"
             , "<EqExpr> :: BoolBinaryOpExpr"
             , "<BoolUnaryOpExpr> :: BoolExpr"
             , "<BoolBinaryOpExpr> :: BoolExpr"

             -- numeric operators producing booleans
             , "GT :: EqOp"
             , "GTE :: EqOp"
             , "LT :: EqOp"
             , "LTE :: EqOp"
             , "EQ :: EqOp"
             , "<NumExpr> <EqOp> <NumExpr> :: EqExpr"
             , "<Ratio> <EqOp> <Fraction> :: EqExpr"
             , "<Ratio> <EqOp> <Ratio> :: EqExpr"
             , "<Var> <EqOp> <Number> :: EqExpr"
             , "<EqExpr> :: BoolExpr"
             ]



{--
  Simple stack-based language that executes boolean expressions defined by the above grammar
--}


-- LANGUAGE DEFINITION

-- values the stack can store
data StackValue =
        -- primitives
          StackNumber Double
        | StackBool Bool 

        -- functions
        | NumOp (Double -> Double -> Double)
        | NumCall (Double -> Double)
        | BoolOp (Bool -> Bool -> Bool)
        | BoolUnaryOp (Bool -> Bool)
        | BoolCompare (Double -> Double -> Bool)


instance Show StackValue where
    show (StackNumber val) = show val
    show (StackBool val) = show val
    show _ = "..."

-- the actual stack
data Stack = Stack [StackValue]

data RunningProgram = RunningProgram Stack (M.Map String StackValue)

-- a stack program
data Program a = Program (RunningProgram -> (a, RunningProgram))

runProgram :: Program a -> RunningProgram -> (a, RunningProgram)
runProgram (Program p) state = p state

instance Monad Program where
    return val = Program identity
                 where
                   identity initState = (val, initState)
    p1 >>= p2 = Program chainedProgram
        where
          chainedProgram initState =
              let (result, endState) = runProgram p1 initState
                  in
                    runProgram (p2 result) endState

getProgramState :: Program RunningProgram
getProgramState = Program $ \state -> (state, state)

putProgramState :: RunningProgram -> Program ()
putProgramState newState = Program $ \state -> ((), newState)


push :: StackValue -> Program ()
push val = Program $ \(RunningProgram (Stack elts) vars) -> ((), RunningProgram (Stack $ val:elts) vars)


pop :: Program StackValue
pop = Program $ \(RunningProgram (Stack elts) vars) -> (head elts, RunningProgram (Stack $ tail elts) vars)


lookupVar :: String -> Program ()
lookupVar name = do RunningProgram _ vars <- getProgramState
                    let varValue = vars M.! name
                    push varValue
                    return ()


numberOp :: Program ()
numberOp = do NumOp func <- pop
              StackNumber y <- pop
              StackNumber x <- pop
              push $ StackNumber (x `func` y)


numberCall :: Program ()
numberCall = do NumCall func <- pop
                StackNumber arg <- pop
                push $ StackNumber (func arg)


boolOp :: Program ()
boolOp = do BoolOp func <- pop
            StackBool y <- pop
            StackBool x <- pop
            push $ StackBool (x `func` y)


boolUnaryOp :: Program ()
boolUnaryOp = do BoolUnaryOp func <- pop
                 StackBool x <- pop
                 push $ StackBool (func x)


boolCompare :: Program ()
boolCompare = do BoolCompare cmp <- pop
                 StackNumber y <- pop
                 StackNumber x <- pop
                 push $ StackBool (x `cmp` y)



-- COMPILER 
-- compiles syntax trees into the Program monad


-- primitives & operators
compile :: SyntaxTree -> Program ()

compile (Leaf "r") = lookupVar "r"
compile (Leaf "g") = lookupVar "g"
compile (Leaf "b") = lookupVar "b"
compile (Leaf "h") = lookupVar "h"
compile (Leaf "s") = lookupVar "s"
compile (Leaf "v") = lookupVar "v"

compile (Leaf "+") = push $ NumOp (+)
compile (Leaf "-") = push $ NumOp (-)
compile (Leaf "*") = push $ NumOp (*)
compile (Leaf "/") = push $ NumOp (\x y -> if y == 0 then fromIntegral (maxBound :: Int) else x/y)

compile (Leaf "abs") = push $ NumCall abs
compile (Leaf "sin") = push $ NumCall sin
compile (Leaf "cos") = push $ NumCall cos
compile (Leaf "tan") = push $ NumCall tan

compile (Leaf "AND") = push $ BoolOp (&&)
compile (Leaf "OR") = push $ BoolOp (||)
compile (Leaf "NOT") = push $ BoolUnaryOp (not)

compile (Leaf "GT") = push $ BoolCompare (>)
compile (Leaf "GTE") = push $ BoolCompare (>=)
compile (Leaf "LT") = push $ BoolCompare (<)
compile (Leaf "LTE") = push $ BoolCompare (<=)
compile (Leaf "EQ") = push $ BoolCompare (==)

compile (Leaf n) = push $ StackNumber (read n)


-- combinators

compile (Branch _ (PrimitiveType "NumExpr") [expr]) = compile expr

compile (Branch _ (PrimitiveType "Var") [var]) = compile var
compile (Branch _ (PrimitiveType "Fraction") [num]) = compile num
compile (Branch _ (PrimitiveType "Number") [num]) = compile num
compile (Branch _ (PrimitiveType "NumOp") [op]) = compile op

compile (Branch _ (PrimitiveType "Ratio") (lhs:op:rhs:[])) = do compile lhs
                                                                compile rhs
                                                                compile op
                                                                numberOp

compile (Branch _ (PrimitiveType "NumOpExpr") (lhs:op:rhs:[])) = do compile lhs
                                                                    compile rhs
                                                                    compile op
                                                                    numberOp

compile (Branch _ (PrimitiveType "NumFunc") [func]) = compile func
compile (Branch _ (PrimitiveType "NumCall") (func:arg:[])) = do compile arg
                                                                compile func
                                                                numberCall

compile (Branch _ (PrimitiveType "BoolOp") [op]) = compile op
compile (Branch _ (PrimitiveType "BoolUnaryOp") [op]) = compile op
compile (Branch _ (PrimitiveType "NumUnaryOp") [op]) = compile op
compile (Branch _ (PrimitiveType "BoolUnaryOpExpr") (op:expr:[])) = do compile expr
                                                                       compile op
                                                                       boolUnaryOp

compile (Branch _ (PrimitiveType "BoolBinaryOpExpr") (lhs:op:rhs:[])) = do compile lhs
                                                                           compile rhs
                                                                           compile op
                                                                           boolOp

compile (Branch _ (PrimitiveType "BoolBinaryOpExpr") [expr]) = do compile expr

compile (Branch _ (PrimitiveType "BoolExpr") [expr]) = compile expr

compile (Branch _ (PrimitiveType "EqOp") [op]) = compile op

compile (Branch _ (PrimitiveType "EqExpr") (lhs:op:rhs:[])) = do compile lhs
                                                                 compile rhs
                                                                 compile op
                                                                 boolCompare

compile other = error $ "Compile pattern matching fail: " ++ show other


{--
  Compiling to Java
--}

toJava :: SyntaxTree -> String

toJava (Leaf "r") = "r"
toJava (Leaf "g") = "g"
toJava (Leaf "b") = "b"
toJava (Leaf "h") = "h"
toJava (Leaf "s") = "s"
toJava (Leaf "v") = "v"

toJava (Leaf "+") = "+"
toJava (Leaf "-") = "-"
toJava (Leaf "*") = "*"
toJava (Leaf "/") = "/"

toJava (Leaf "abs") = "Math.abs"

toJava (Leaf "AND") = "&&"
toJava (Leaf "OR") = "||"
toJava (Leaf "NOT") = "!"

toJava (Leaf "GT") = ">"
toJava (Leaf "GTE") = ">="
toJava (Leaf "LT") = "<"
toJava (Leaf "LTE") = "<="
toJava (Leaf "EQ") = "=="

toJava (Leaf n) = n


toJava (Branch _ (PrimitiveType "NumExpr") [expr]) = toJava expr

toJava (Branch _ (PrimitiveType "Var") [var]) = toJava var
toJava (Branch _ (PrimitiveType "Fraction") [num]) = toJava num
toJava (Branch _ (PrimitiveType "Number") [num]) = toJava num
toJava (Branch _ (PrimitiveType "NumOp") [op]) = toJava op
toJava (Branch _ (PrimitiveType "NumOpExpr") (lhs:op:rhs:[])) = "(" ++ toJava lhs ++ " " ++  toJava op ++ " " ++  toJava rhs ++ ")"
toJava (Branch _ (PrimitiveType "Ratio") (lhs:op:rhs:[])) = "(" ++ toJava lhs ++ " " ++  toJava op ++ " " ++  toJava rhs ++ ")"


toJava (Branch _ (PrimitiveType "NumFunc") [func]) = toJava func
toJava (Branch _ (PrimitiveType "NumCall") (func:arg:[])) = toJava func ++ "(" ++ toJava arg ++ ")"

toJava (Branch _ (PrimitiveType "BoolOp") [op]) = toJava op
toJava (Branch _ (PrimitiveType "BoolUnaryOp") [op]) = toJava op
toJava (Branch _ (PrimitiveType "NumUnaryOp") [op]) = toJava op
toJava (Branch _ (PrimitiveType "BoolUnaryOpExpr") (op:expr:[])) = do "(" ++ toJava op ++ toJava expr ++ ")"

toJava (Branch _ (PrimitiveType "BoolBinaryOpExpr") (lhs:op:rhs:[])) = "(" ++ toJava lhs ++ " " ++  toJava op ++ " " ++ toJava rhs ++ ")"

toJava (Branch _ (PrimitiveType "BoolBinaryOpExpr") [expr]) = toJava expr

toJava (Branch _ (PrimitiveType "BoolExpr") [expr]) = toJava expr

toJava (Branch _ (PrimitiveType "EqOp") [op]) = toJava op

toJava (Branch _ (PrimitiveType "EqExpr") (lhs:op:rhs:[])) = "(" ++ toJava lhs ++ " " ++  toJava op ++ " " ++ toJava rhs ++ ")"

toJava other = error $ "ToJava pattern matching fail: " ++ show other



{--
  INTERPRETER
--}

interpret :: Program a -> M.Map String StackValue -> StackValue
interpret program variables = let (_, RunningProgram stack _) = runProgram program $ RunningProgram (Stack []) variables
                                  (Stack elts) = stack
                              in
                                if length elts == 1
                                then
                                    head elts
                                else
                                    error $ "Stack has more or less than 1 element: " ++ show (length elts)


{--
  GP integration
--}

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

sampleCount = 1000

fitnessEvaluator :: TrainingData -> EvaluatedSyntaxTree -> (TrainingData, Double)
fitnessEvaluator td atree =
    let prog = compile $ tree atree
        (samples, endState) = randomSamples td sampleCount
        matches = Data.List.map (\(r, g, b, h, s, v, expected) ->
                                     let actual = eval prog r g b h s v
                                     in
                                       if actual == expected
                                       then 1.0
                                       else 0.0)
                  samples

        agreement = sum matches / (fromIntegral $ length samples)
     in
       (endState, agreement)
    where
--      eval :: Program a -> Int -> Int -> Int -> Bool
      eval prog r g b h s v =
          let StackBool val = interpret prog $
                              M.fromList [("r", StackNumber $ fromIntegral r)
                                         , ("g", StackNumber $ fromIntegral g)
                                         , ("b", StackNumber $ fromIntegral b)
                                         , ("h", StackNumber h)
                                         , ("s", StackNumber s)
                                         , ("v", StackNumber v)
                                         ]
          in
            val


reporter :: EvolutionReporter a b c
reporter generation state trees params =
    do let bestTree = bestMember trees
       putStrLn $ "Generation " ++ show generation
                    ++ ", fitness=" ++ show (fitness bestTree)
       putStrLn $ toJava (tree bestTree)
       putStrLn ""
       return (state, params)

elitistSelector :: GenerationMerger state
elitistSelector state oldPopulation newPopulation = (best ++ rest, state)
    where
      size = length oldPopulation
      ordered = reverse $ sort (oldPopulation ++ newPopulation)
      best = take (round ((fromIntegral size) / 100)) ordered
      rest = take (size - length best) newPopulation


loadPixelData :: FilePath -> IO ([PixelSample])
loadPixelData path = do contents <- readFile path
                        return (read contents :: [PixelSample])


type PixelSample = (Int, Int, Int, Double, Double, Double, Bool)
data TrainingData = TrainingData (Array Int PixelSample) [Int]

randomSamples :: TrainingData -> Int -> ([PixelSample], TrainingData)
randomSamples td 0 = ([], td)
randomSamples td@(TrainingData pixels rands) count = let (restSamples, TrainingData _ restRands) = randomSamples td (count - 1)
                                                         thisSample = chooseArray restRands pixels
                                                     in
                                                       (thisSample:restSamples, TrainingData pixels $ tail restRands)


runGA :: IO ()
runGA =
    do args <- getArgs
       seed <- getStdRandom random :: IO (Int)
       pixelData <- loadPixelData (args !! 1)
       putStrLn $ "Pixel data points: " ++ (show $ length pixelData)
       let rs = randoms (mkStdGen seed) :: [Int]
           Good grammar = getGrammar
           params = Parameters { 
                                grammar = grammar
                              , maxGenerationDepth = 20
                              , mutationProbability = 0.3
                              , evaluator = makePureEvaluator fitnessEvaluator True
                              , populationSize = read $ args !! 0
                              , merger = elitistSelector
                              , stopCondition = \trees -> fitness (bestMember trees) > 1
                              , userGenerators = M.fromList [("num", (numGenerator, randomRs (-300.0, 300.0) (mkStdGen $ rs !! 2)))
                                                            , ("fraction", (numGenerator, randomRs (0.0, 1.0) (mkStdGen $ rs !! 4)))]
                              , completeMutationProbability = 0.1
                              , randomSelectionProbability = 0.1
                              , crossoverHooks = M.fromList [("crossover", (crossover, []))]
                              , select = tournament 40
                              , seed = rs !! 0
                              }
           initTrainingState = TrainingData (listArray (0, length pixelData) pixelData) (randoms (mkStdGen $ rs !! 3) :: [Int])
       runGP params initTrainingState (NonterminalTerm (PrimitiveType "BoolExpr") False "") 500 reporter
       return ()


main :: IO ()
main = runGA


testProgram :: Program ()
testProgram = do push $ StackNumber 2
                 push $ StackNumber 10
                 push $ StackNumber 3
                 push $ NumOp (-)
                 numberOp
                 push $ NumOp (*)
                 numberOp
                 push $ StackNumber 42
                 push $ BoolCompare (<)
                 boolCompare
