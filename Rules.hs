{--
  Defines datatypes for expressing metalanguage rules
--}

import Debug.Trace (trace)
import System.Random (mkStdGen, randoms)
import Control.Monad (mapM, filterM)
import Data.List (intersperse, find)
import Data.Map (Map, fromList, (!), member, insert)
import Types
import AbstractTypeMatching


data Possibly a = Good a
                | Error String
                  deriving (Show)

instance Monad Possibly where
    (Good val)  >>= f   = f val
    (Error val) >>= f   = Error val
    return val          = Good val

quote :: String -> String
quote str = "'" ++ str ++ "'"

-- represents a term of the name::type
data Term = NonterminalTerm { termName :: String, termRequiredType :: Type } 
          | TerminalTerm    { termName :: String }

isNonterminal :: Term -> Bool
isNonterminal (NonterminalTerm _ _) = True
isNonterminal _ = False

isTerminal = not . isNonterminal

instance Show Term where
    show (NonterminalTerm name requiredType) = name ++ "::" ++ show requiredType
    show (TerminalTerm name)                 = "\"" ++ name ++ "\""

-- represents one possible expansion of a nonterminal
data Expansion = Expansion { expansionTerms :: [Term], expansionType :: Type }

instance Show Expansion where
    show (Expansion terms expansionType) = "{ " ++ (concat . intersperse " " . map show) terms ++ " }::"
                                           ++ show expansionType


-- represents a grammar rule, e.g. expr -> expr + expr
data Rule = Rule { ruleName :: String, ruleExpansions :: [Expansion] }


-- returns rules with the same name as the given term's id, provided that term is a nonterminal
findRule :: [Rule] -> Term -> Possibly Rule
findRule rules t@(TerminalTerm _) = Error $ "Attempted to find a rule for terminal " ++ show t
findRule rules (NonterminalTerm name _) = maybe (Error $ "Could not find rule named " ++ quote name)
                                          Good
                                          $ find ((name ==) . ruleName) rules


-- represents syntax trees generated from the meta grammar
data SyntaxTree = Branch { branchTerm :: Term,                  -- the term that the branch was expanded from
                           branchType :: Type,                  -- type of the branch
                           branchChildren :: [SyntaxTree] }     -- subtrees
                | Leaf   { leafValue :: String }
                | Null                                          -- special subtree type denoting a subtree
                                                                -- which was not generated due to unstatisfiability
                                                                -- of type constraints

isLeaf, isBranch :: SyntaxTree -> Bool
isLeaf (Leaf _) = True
isLeaf _  = False

isBranch = not . isLeaf


instance Show SyntaxTree where
    show = ("\n" ++) . printSyntaxTree 1


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
                               (Branch term t _)      -> termName term ++ "::" ++ show t
                                                         ++ " (" ++ show (termRequiredType term) ++ ")"
                                                       


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

startState :: [Expansion] -> [Int] -> Int -> GeneratorState
startState expansions choices maxDepth = GeneratorState {
                                           stateExpansions = expansions
                                         , stateVariableMap = fromList []
                                         , stateChoices = choices
                                         , stateCount = 0
                                         , stateDepth = 0
                                         , stateMaxDepth = maxDepth
                                         }

-- gets all expansions for the given list of rules
expansions :: [Rule] -> [Expansion]
expansions = concat . map ruleExpansions


-- removes the head choice from a GeneratorState
advance :: GeneratorState -> GeneratorState
advance state = state { stateChoices = tail $ stateChoices state, stateCount = (+1) $ stateCount state }

-- creates an error string indicating type mismatch
showTypeMismatch :: Type -> Type -> String
showTypeMismatch expected actual = "Type mismatch. Expected: " ++ show expected ++ ". Actual: " ++ show actual

instantiate :: Type -> Type -> GeneratorState -> GeneratorState
instantiate originalType newType originalState =
    case originalType of
      (PrimitiveType _)      -> originalState
      v@(TypeVariable  _)    -> originalState { stateVariableMap = insert v newType $ stateVariableMap originalState }
      (PolymorphicType _ _) -> foldr instantiator originalState $ zip (vars originalType) (vars newType)
                               where
                                 instantiator = (\(before, after) state ->
                                                     if member before (stateVariableMap state) && before /= after
                                                     then error $ showTypeMismatch ((stateVariableMap state) ! before)
                                                                                   after
                                                     else instantiate before after state
                                                )

deepen :: GeneratorState -> GeneratorState
deepen state = state { stateDepth = 1 + stateDepth state }


-- a `isTypeCompatible` b iff a can be substituted for b
isTypeCompatible :: Type -> Type -> Bool
(PrimitiveType nameA) `isTypeCompatible` (PrimitiveType nameB) = nameA == nameB
(TypeVariable _)      `isTypeCompatible` _                     = True
_                     `isTypeCompatible` (TypeVariable _)      = True
(PolymorphicType nameA varsA) `isTypeCompatible` (PolymorphicType nameB varsB) = 
    nameA == nameB && length varsA == length varsB && all (\(a,b) -> a `isTypeCompatible` b) (zip varsA varsB)
_ `isTypeCompatible` _ = False


-- gets all expansions that can be substituted for the given type
compatibleExpansions :: Type -> [Expansion] -> [Expansion]
compatibleExpansions t = filter $ (`isTypeCompatible` t) . expansionType


-- chooses a type-compatible expansion for the given type, if any
chooseExpansion :: GeneratorState -> Type -> Possibly (Expansion, GeneratorState)
chooseExpansion state t = if null exs
                          then Error $ "Could not find a compatible expansion for type " ++ show t
                          else Good (exs !! (head . stateChoices) state, advance state)
                              where exs = compatibleExpansions t (stateExpansions state)


-- expands a list of terms, modifying the state accordingly
expandTerms :: GeneratorState -> [Term] -> Possibly ([SyntaxTree], GeneratorState)
expandTerms state [] = Good ([], state)
expandTerms state (t:ts) = expand state t >>=
                           (\(tree, nextState) -> expandTerms state ts >>=
                            (\(trees, finalState) -> Good $ (tree:trees, finalState)))

-- expands a single term
expand :: GeneratorState -> Term -> Possibly (SyntaxTree, GeneratorState)
expand state (TerminalTerm name) = Good (Leaf name, state)
expand initState term@(NonterminalTerm name requiredType)
           | stateDepth initState >= stateMaxDepth initState = Error "Maximum depth exceeded"
           | otherwise = chooseExpansion initState requiredType >>=
                         (\(expansion, state) -> Error "not yet implemented")

typeA = (PolymorphicType "List" [(PolymorphicType "Func"
                                 [(TypeVariable "a"), (PolymorphicType "Func" [TypeVariable "b", TypeVariable "c"])])])

typeB = (PolymorphicType "List" [(PolymorphicType "Func"
                                 [(PrimitiveType "Number"),
                                  (PolymorphicType "Func" [PrimitiveType "String",
                                                           PolymorphicType "List" [PrimitiveType "String"]])])])

typeC = (PolymorphicType "List" [(PolymorphicType "Func"
                                 [(TypeVariable "a"), (PolymorphicType "Func" [TypeVariable "b", TypeVariable "a"])])])