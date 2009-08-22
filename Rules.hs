{--
  Defines datatypes for expressing metalanguage rules
--}

import Debug.Trace (trace)
import System.Random (mkStdGen, randoms)
import Control.Monad (mapM, filterM)
import Data.List (intersperse, find)
import Data.Map (Map, fromList, (!), member)
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
data SyntaxTree = Branch { branchTerm :: Term,            -- the term that the branch was expanded from
                           children :: [SyntaxTree] }     -- subtrees
                | Leaf   { leafValue :: String }
                | Null                                    -- special subtree type denoting a subtree
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
                               (Branch term children) ->
                                   "- " ++ header ++ "\n" ++
                                   (concat . map (identString ++)) subtrees
                                 where
                                   identString = replicate ident '\t'
                                   subtrees = (intersperse "\n" . map (printSyntaxTree (ident + 1))) children
                                

-- prints the header of a syntax tree, without any leading or trailing whitespace characters
printSyntaxTreeHeader :: SyntaxTree -> String
printSyntaxTreeHeader tree = case tree of
                               (Leaf value)         -> "\"" ++ value ++ "\""
                               (Branch term _)      -> show term
                                                       


-- holds state necessary for syntax tree generation
data GeneratorState = GeneratorState
    {
      stateTypes      :: [Type]           -- all types available to the generator
    , stateRules      :: [Rule]           -- all rules available to the generator
    , stateChoices    :: [Int]            -- an infinite list denoting which expansion to choose at each step
    , stateCount      :: Int              -- number of times state has been changed, used for debugging
    }

-- removes the head choice from a GeneratorState
advance :: GeneratorState -> GeneratorState
advance state = state { stateChoices = tail $ stateChoices state, stateCount = (+1) $ stateCount state }

isTypeCompatible :: Type -> Type -> Bool
(PrimitiveType nameA) `isTypeCompatible` (PrimitiveType nameB) = nameA == nameB
(TypeVariable _) `isTypeCompatible` _ = True
_ `isTypeCompatible` (TypeVariable _) = True
(PolymorphicType nameA varsA) `isTypeCompatible` (PolymorphicType nameB varsB) = 
    nameA == nameB && length varsA == length varsB && all (\(a,b) -> a `isTypeCompatible` b) (zip varsA varsB)
_ `isTypeCompatible` _ = False


compatibleExpansions :: [Expansion] -> Type -> [Expansion]
compatibleExpansions = undefined

-- expands a single term
expand :: GeneratorState -> Term -> Possibly (SyntaxTree, GeneratorState)
expand state (TerminalTerm name) = Good (Leaf name, state)
-- expand state (NonterminalTerm name requiredType) =