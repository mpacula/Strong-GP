{--
  Defines datatypes for expressing metalanguage rules
-}


module Generator
    (
      isNonterminal
    , isTerminal
    , Term (..)
    , Expansion (..)
    , hasNonterminals
    ) where


import Debug.Trace (trace)
import System.Random (mkStdGen, randoms, getStdGen)
import Data.List (intersperse, find)
import Data.Map (Map, fromList, (!), member, insert)
import Types (Type (..), isPrimitive, isPolymorphic, isTypeVariable)
import Possibly (Possibly (Good, Error))
import Utils (choose)



{-
  TERMINALS & NONTERMINALS
-}



data Term = NonterminalTerm { termRequiredType :: Type } 
          | TerminalTerm    { termName :: String }


instance Show Term where
    show (NonterminalTerm requiredType) = show requiredType
    show (TerminalTerm name)            = "\"" ++ name ++ "\""


isNonterminal,isTerminal :: Term -> Bool
isNonterminal (NonterminalTerm _) = True
isNonterminal _                   = False

isTerminal = not . isNonterminal



{-
  EXPANSIONS
-}



-- represents one possible expansion of a nonterminal
data Expansion = Expansion
    {
      expansionTerms :: [Term]
    , expansionType :: Type
    }


hasNonterminals :: Expansion -> Bool
hasNonterminals = any isNonterminal . expansionTerms


instance Show Expansion where
    show (Expansion terms expansionType) = "{ " ++ (concat . intersperse " " . map show) terms ++ " }::"
                                           ++ show expansionType



{-
  SYNTAX TREES
-}



-- represents syntax trees generated from the meta grammar
data SyntaxTree = Branch { branchTerm :: Term,                  -- the term that the branch was expanded from
                           branchType :: Type,                  -- type of the branch
                           branchChildren :: [SyntaxTree] }     -- subtrees
                | Leaf   { leafValue :: String }


instance Show SyntaxTree where
    show = ("\n" ++) . printSyntaxTree 1


-- shows terminals from all leaves separated by spaces
flattenTree :: SyntaxTree -> String
flattenTree (Leaf value) = value
flattenTree (Branch _ _ children) = (concat . intersperse " " . map flattenTree) children


-- pretty prints a syntax tree
printSyntaxTree :: Int -> SyntaxTree -> String
printSyntaxTree ident tree = let header = printSyntaxTreeHeader tree in
                             case tree of                                
                               (Leaf value)                -> "- " ++ header
                               (Branch term t children) ->
                                   "- " ++ header ++ "\n" ++
                                   (concat . map (identString ++)) subtrees
                                 where
                                   identString = replicate ident '\t'
                                   subtrees = (intersperse "\n" . map (printSyntaxTree (ident + 1))) children
                                

-- prints the header of a syntax tree, without any leading or trailing whitespace characters
printSyntaxTreeHeader :: SyntaxTree -> String
printSyntaxTreeHeader tree = case tree of
                               (Leaf value)         -> "\"" ++ value ++ "\""
                               (Branch term t _)      -> show t ++ ", from " ++ show (termRequiredType term)



{-
  SYNTAX TREE GENERATION
-}



-- creates an error string indicating type mismatch
showTypeMismatch :: Type -> Type -> String
showTypeMismatch expected actual = "Type mismatch. Expected: " ++ show expected ++ ". Actual: " ++ show actual


-- holds state necessary for syntax tree generation
data GeneratorState = GeneratorState
    {
      stateExpansions   :: [Expansion]      -- all expansions available to the generator
    , stateVariableMap  :: Map Type Type    -- maps instantiated type variables to concrete types
    , stateChoices      :: [Int]            -- an infinite list denoting which expansion to choose at each step
    , stateCount        :: Int              -- number of times state has been changed, used for debugging
    , stateDepth        :: Int              -- current depth in the tree
    , stateMaxDepth     :: Int              -- maximum tree depth
    }



{-
  STATE TRANSFORMATIONS
-}



-- creates an initial state that can be used to generate syntax trees
startState :: [Expansion] -> [Int] -> Int -> GeneratorState
startState expansions choices maxDepth = GeneratorState {
                                           stateExpansions = expansions
                                         , stateVariableMap = fromList []
                                         , stateChoices = choices
                                         , stateCount = 0
                                         , stateDepth = 0
                                         , stateMaxDepth = maxDepth
                                         }

-- removes the head choice from a GeneratorState
advanceChoices :: GeneratorState -> GeneratorState
advanceChoices state = state { stateChoices = tail $ stateChoices state, stateCount = (+1) $ stateCount state }


-- clears type variable map in a generator state
clearInstances :: GeneratorState -> GeneratorState
clearInstances state = state { stateVariableMap = fromList [] }


-- increased the depth in a geneartor state
deepen :: GeneratorState -> GeneratorState
deepen state = state { stateDepth = 1 + stateDepth state }


-- copies choices from one state to another
copyChoices :: GeneratorState -> GeneratorState -> GeneratorState
copyChoices src dest = dest { stateChoices = stateChoices src, stateCount = stateCount src  }


-- given a state at one level, creates state that can be used for generating children one
-- level below
childrenState :: GeneratorState -> GeneratorState
childrenState = deepen . clearInstances


-- given an original type and a new one, creates type variable bindings in the generator state
-- that would transform the first into the latter 
instantiateState :: Type -> Type -> GeneratorState -> GeneratorState
instantiateState originalType newType originalState =
    case originalType of
      (PrimitiveType _)      -> originalState
      v@(TypeVariable  _)    -> case newType of
                                  (TypeVariable _) -> originalState
                                  _                -> originalState {
                                                        stateVariableMap = insert v newType $ stateVariableMap originalState
                                                      }
      (PolymorphicType _ _)  -> foldr instantiator originalState $ zip (vars originalType) (vars newType)
                               where
                                 -- Creates type variable bindings in generator state
                                 -- before: type before instantiation
                                 -- after:  type after instantiation
                                 instantiator :: (Type, Type) -> GeneratorState -> GeneratorState
                                 instantiator = (\(before, after) state ->
                                                     let varMap = (stateVariableMap state)
                                                     in
                                                       -- is the variable already bound?
                                                       if member before varMap && (varMap ! before) /= after
                                                       then error $ showTypeMismatch (varMap ! before) after
                                                       else instantiateState before after state
                                                )

-- subtitutes type variables in a type using variable map from a generator state
instantiateType :: GeneratorState -> Type -> Type
instantiateType state t = case t of
                            t@(PrimitiveType _)         -> t
                            t@(TypeVariable _ )         -> if member t (stateVariableMap state)
                                                           then (stateVariableMap state) ! t
                                                           else t
                            (PolymorphicType name vars) -> (PolymorphicType name (map (instantiateType state) vars))


-- a `isTypeCompatible` b iff a can be substituted for b
isTypeCompatible :: Type -> Type -> Bool
(PrimitiveType nameA)         `isTypeCompatible` (PrimitiveType nameB)         = nameA == nameB
(TypeVariable _)              `isTypeCompatible` _                             = True
_                             `isTypeCompatible` (TypeVariable _)              = True

(PolymorphicType nameA varsA) `isTypeCompatible` (PolymorphicType nameB varsB) = 
    nameA        == nameB             &&
    length varsA == length varsB      && 
    all (\(a, b) -> a `isTypeCompatible` b) (zip varsA varsB)

_                              `isTypeCompatible` _                            = False


-- gets all expansions that can be substituted for the given type
compatibleExpansions :: Type -> [Expansion] -> [Expansion]
compatibleExpansions t = filter $ (`isTypeCompatible` t) . expansionType


-- chooses a type-compatible expansion for the given type, if any
chooseExpansion :: GeneratorState -> Type -> Possibly (Expansion, GeneratorState)
chooseExpansion state t = if null exs
                          then Error $ "Could not find a compatible expansion for type " ++ show t ++
                                   " at depth " ++ show (stateDepth state)
                          else Good $ (instantiateExpansion state t (choose (stateChoices state) exs), advanceChoices state)
                              where allCompatible = compatibleExpansions t (stateExpansions state)
                                    exs = if (stateDepth state) == (stateMaxDepth state) - 1
                                          then filter (not . hasNonterminals) allCompatible
                                          else allCompatible


-- expands a list of terms, modifying the state accordingly
expandTerms :: GeneratorState -> [Term] -> Possibly ([SyntaxTree], GeneratorState)
expandTerms state [] = Good ([], state)
expandTerms state (t:ts) = expand state t >>=
                           (\(tree, nextState) -> expandTerms nextState ts >>=
                            (\(trees, finalState) -> Good $ (tree:trees, finalState)))


-- given a required type for an expansion, substitutes concrete values for type variables in that expansion
-- If both expansions share type variable names, they will be renamed before substitution
instantiateExpansion :: GeneratorState -> Type -> Expansion -> Expansion
instantiateExpansion state requiredType ex = Expansion terms $ (instantiateType newState . expansionType) ex
                                             where
                                               newState = instantiateState (expansionType ex) requiredType state
                                               terms = map termMapper (expansionTerms ex)
                                                       
                                               termMapper t@(TerminalTerm _) = t
                                               termMapper (NonterminalTerm originalType) =
                                                   NonterminalTerm $ instantiateType newState (makeVariablesDistinct requiredType originalType)


-- given 2 types, renames variable types in the second one so that the two have none in common
makeVariablesDistinct :: Type -> Type -> Type
makeVariablesDistinct other this = case this of
                                     (TypeVariable name) -> (TypeVariable $ uniqueName name 2 otherNames)
                                     (PolymorphicType name vars) -> (PolymorphicType name (map (makeVariablesDistinct other) vars))
                                     t -> t
                                   where
                                     otherNames = variableNames other
                                     

-- gets names of all type variables in a type. The list can contain duplicates
variableNames :: Type -> [String]
variableNames (PrimitiveType _) = []
variableNames (TypeVariable name) = [name]
variableNames (PolymorphicType _ vars) = (concat . map variableNames) vars


-- given a prototype name and a suffix, increments the suffix as many times as necessary to make
-- (prototype ++ show suffix) name distinct from any of the names in a given list
uniqueName :: String -> Int -> [String] -> String
uniqueName prototype suffix others = if try `notElem` others
                                     then try
                                     else uniqueName prototype (suffix + 1) others
                                         where
                                           try = prototype ++ show suffix


-- generates a syntax tree which is type-compatible with the given term
expand :: GeneratorState -> Term -> Possibly (SyntaxTree, GeneratorState)
expand state (TerminalTerm name) = Good (Leaf name, state)
expand initState term@(NonterminalTerm requiredType)
           | stateDepth initState >= stateMaxDepth initState = Error "Maximum depth exceeded"
           | otherwise =
               do
                 (expansion, state)             <- chooseExpansion initState (instantiateType initState requiredType)
                 (children, childrenFinalState) <- expandTerms (childrenState state) (expansionTerms expansion)
                                                   
                 let finalExpansionType = instantiateType childrenFinalState (expansionType expansion)
                                          
                 Good (Branch term finalExpansionType children,
                       instantiateState requiredType finalExpansionType (copyChoices childrenFinalState initState))



{-
  DEBUG DEFINITIONS
-}



typeA = (PolymorphicType "List" [(PolymorphicType "Func"
                                 [(TypeVariable "a"), (PolymorphicType "Func" [TypeVariable "b", TypeVariable "c"])])])

typeB = (PolymorphicType "List" [(PolymorphicType "Func"
                                 [(PrimitiveType "Number"),
                                  (PolymorphicType "Func" [PrimitiveType "String",
                                                           PolymorphicType "List" [PrimitiveType "String"]])])])

typeC = (PolymorphicType "List" [(PolymorphicType "Func"
                                 [(TypeVariable "a"), (PolymorphicType "Func" [TypeVariable "b", TypeVariable "a"])])])

tNumber = PrimitiveType "Number"
tString = PrimitiveType "String"

vA = TypeVariable "a"
vB = TypeVariable "b"
vC = TypeVariable "c"

tList = PolymorphicType "List" [vA]

expansion1 = (Expansion [(NonterminalTerm tNumber), (TerminalTerm "+"), (NonterminalTerm tNumber)] tNumber)
expansion2 = (Expansion [(NonterminalTerm tNumber), (TerminalTerm "*"), (NonterminalTerm tNumber)] tNumber)

expansion3 = Expansion [(TerminalTerm "1")] tNumber
expansion4 = Expansion [(TerminalTerm "2")] tNumber

expansion5 = (Expansion [(NonterminalTerm tString), (TerminalTerm "++"), (NonterminalTerm tString)] tString)
expansion6 = (Expansion [(NonterminalTerm vA), (TerminalTerm ":"), (NonterminalTerm tList)] tList)
expansion7 = (Expansion [TerminalTerm "[]"] tList)

expansion8 = (Expansion [TerminalTerm "hello"] tString)
expansion9 = (Expansion [TerminalTerm "world"] tString)

allExpansions = [expansion1, expansion2, expansion3, expansion4, expansion5, expansion6, expansion7, expansion8, expansion9]
testState = startState allExpansions (randoms (mkStdGen 12) :: [Int]) 10

mkFunc :: Type -> Type -> Type
mkFunc arg ret = PolymorphicType "Func" [arg, ret]

lparen = TerminalTerm "("
rparen = TerminalTerm ")"

sexp1 = Expansion [lparen, (NonterminalTerm (mkFunc vA vB)), (NonterminalTerm vA), rparen] vB
sexp2 = Expansion [lparen, (TerminalTerm "lambda"), lparen, (TerminalTerm "x"), rparen,
                             (NonterminalTerm vB), rparen] (mkFunc vA vB)
sexp3 = Expansion [lparen, TerminalTerm "+", (NonterminalTerm tNumber), (NonterminalTerm tNumber), rparen] tNumber
sexp4 = Expansion [lparen, TerminalTerm "-", (NonterminalTerm tNumber), (NonterminalTerm tNumber), rparen] tNumber
sexp5 = Expansion [lparen, TerminalTerm "*", (NonterminalTerm tNumber), (NonterminalTerm tNumber), rparen] tNumber
sexp6 = Expansion [lparen, TerminalTerm "/", (NonterminalTerm tNumber), (NonterminalTerm tNumber), rparen] tNumber

sexp7 = Expansion [lparen, TerminalTerm "cons", NonterminalTerm vA, NonterminalTerm tList] tList

sexp8 = Expansion [(TerminalTerm "1")] tNumber
sexp9 = Expansion [(TerminalTerm "2")] tNumber
sexp10 = Expansion [(TerminalTerm "3")] tNumber
sexp11 = Expansion [(TerminalTerm "4")] tNumber

sexps = [sexp1, sexp2, sexp3, sexp4, sexp5, sexp6, sexp7, sexp8, sexp9, sexp10, sexp11]
sexpState = startState sexps (randoms (mkStdGen 1) :: [Int]) 5