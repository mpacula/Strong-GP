{--
  Defines datatypes for expressing metalanguage rules
--}

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

-- represents a term of the form name@id::type, e.g. fst_expr@expr::AnyType
data Term = NonterminalTerm { termName :: String, termId :: String, termRequiredType :: Type } 
          | TerminalTerm    { termName :: String }

isNonterminal :: Term -> Bool
isNonterminal (NonterminalTerm _ _ _) = True
isNonterminal _ = False

isTerminal = not . isNonterminal

instance Show Term where
    show (NonterminalTerm name id requiredType) = nameString ++ id ++ "::(" ++ show requiredType ++ ")"
                                                  where
                                                    nameString = if null name
                                                                 then ""
                                                                 else name ++ "@"
    show (TerminalTerm name) = "\"" ++ name ++ "\""

-- represents one possible expansion of a nonterminal
data Expansion = Expansion { expansionTerms :: [Term], expansionTypeExpr :: TypeExpression }

instance Show Expansion where
    show (Expansion terms typeExpr) = "{ " ++ (concat . intersperse " " . map show) terms ++ " }::"
                                      ++ show typeExpr

-- gets all expansions compatible with the given type
compatibleExpansions :: [Type] -> Type -> [Expansion] -> Possibly [Expansion]
compatibleExpansions types t exs = filterM (isExpansionCompatible types t) exs


-- checks whether an expansion is compatible with the given type
isExpansionCompatible :: [Type] -> Type -> Expansion -> Possibly Bool
isExpansionCompatible types t expansion =
    evalTypeExpression types (expansionTypeMap expansion) (expansionTypeExpr expansion) >>=
    (\expansionType -> Good $ expansionType `instanceOf` t)

-- creates a map from names to types in an expansion
expansionTypeMap :: Expansion -> Map String Type
expansionTypeMap expansion = fromList $ zip names types
                             where
                               names = map termName nonterminals
                               types = map termRequiredType nonterminals
                               nonterminals = (filter (not . null . termName) . filter isNonterminal)
                                              (expansionTerms expansion)

-- represents a type expression, e.g. ::String or ::coalesce Number Integer
data TypeExpression = TypeIdentity String                        -- e.g. ::Number
                    | TypeOf String                              -- e.g, ::typeOf my_expr 
                    | TypeCoalesce TypeExpression TypeExpression -- e.g  ::coalesce (typeOf lhs) (typeOf rhs)
                      deriving (Show)

-- evaluates a type expression for the given type map. A type map associates names with types.
evalTypeExpression :: [Type] -> Map String Type -> TypeExpression -> Possibly Type
evalTypeExpression types nameMap expr =
    case expr of
      (TypeIdentity cs) -> maybe (Error (show expr ++ ": unknown type " ++ "\"" ++ cs ++ "\""))
                                 Good
                                 (find (\t -> name t == cs) types)

      expr@(TypeOf name)     -> if member name nameMap
                                then Good $ nameMap ! name
                                else Error (show expr ++ ": symbol \"" ++ name ++ "\"" ++ " not found")
                                            
      (TypeCoalesce exprA exprB) -> evalTypeExpression types nameMap exprA           >>=
                              (\typeA -> evalTypeExpression types nameMap exprB >>=
                                         (\typeB -> Good (typeA, typeB)))           >>=
                              (\(typeA, typeB) -> Good $ coalesce typeA typeB)


-- represents a grammar rule, e.g. expr -> expr + expr
data Rule = Rule { ruleName :: String, ruleExpansions :: [Expansion] }

-- returns rules with the same name as the given term's id, provided that term is a nonterminal
findRule :: [Rule] -> Term -> Possibly Rule
findRule rules t@(TerminalTerm _) = Error $ "Attempted to find a rule for terminal " ++ show t
findRule rules (NonterminalTerm _ id _) = maybe (Error $ "Could not find rule named " ++ quote id)
                                                Good
                                                $ find ((id ==) . ruleName) rules


{--
  Test grammar:
  Rule 1: expr -> {a@atom::Number "+" b@atom::Number}::coalesce a b
  Rule 2: atom -> { "1" } :: Integer
                  { "2" } :: Complex
                  { "hello, world" } :: String
--}

tString = SimpleType "String"
tNumber = SimpleType "Number"
tReal = DerivedType "Real" tNumber
tRational = DerivedType "Rational" tReal
tNatural = DerivedType "Natural" tRational
tInteger = DerivedType "Integer" tRational
tIrrational = DerivedType "Irrational" tReal
tComplex = DerivedType "Complex" tNumber
tNumberNumberRational = ChainType [tNumber, tNumber, tRational]
allTypes = [tString, tNumber, tReal, tRational, tNatural, tInteger, tIrrational, tComplex, tNumberNumberRational]

expansion1 = Expansion
                     [
                      (NonterminalTerm "a" "atom" tNumber),
                      (TerminalTerm "+"),
                      (NonterminalTerm "b" "atom" tComplex)
                     ]
             (TypeCoalesce (TypeOf "a") (TypeOf "b"))

rule1 = Rule "expr" [expansion1]

rule2 = Rule "atom" [Expansion [TerminalTerm "1"] (TypeIdentity "Integer"),
                     Expansion [TerminalTerm "2"] (TypeIdentity "Complex"),
                     Expansion [TerminalTerm "hello, world"] (TypeIdentity "String")]

genState = GeneratorState allTypes [rule1, rule2] [1..100]


-- represents syntax trees generated from the meta grammar
data SyntaxTree = Branch { branchTerm :: Term,            -- the term that the branch was expanded from
                           branchExpansion :: Expansion,  -- the expansion used to expand the branch
                           branchType :: Type,            -- type of the subtree rooted at the branch
                           children :: [SyntaxTree] }     -- subtrees
                | Leaf   { leafValue :: String }

isLeaf, isBranch :: SyntaxTree -> Bool
isLeaf (Leaf _) = True
isLeaf _  = False

isBranch = not . isLeaf


instance Show SyntaxTree where
    show = printSyntaxTree 0


printSyntaxTree :: Int -> SyntaxTree -> String
printSyntaxTree ident tree = case tree of
                               (Leaf value) -> "\"" ++ value ++ "\""
                               (Branch term ex t children) ->
                                   show term ++  " => " ++ show (expansionTypeExpr ex) ++ " = " ++ show t ++ "\n"
                                   ++ identString ++ "\t- "
                                          ++ (concat . intersperse "\n\t- ")
                                                 (map (printSyntaxTree (ident + 1)) children)
                              where identString = replicate ident '\t'


-- holds state necessary for syntax tree generation
data GeneratorState = GeneratorState
    {
      stateTypes      :: [Type]           -- all types available to the generator
    , stateRules      :: [Rule]           -- all rules available to the generator
    , stateChoices    :: [Int]            -- an infinite list denoting which expansion to choose at each step
    }

-- removed the head choice from a GeneratorState
advance :: GeneratorState -> GeneratorState
advance state = state { stateChoices = tail $ stateChoices state }

-- chooses an expansion compatible with the given term, if any
chooseExpansion :: GeneratorState -> Term -> Possibly (GeneratorState, Expansion)
chooseExpansion state t@(NonterminalTerm _ _ requiredType) =
    findRule (stateRules state) t                                                         >>=
    (\rule -> compatibleExpansions (stateTypes state) requiredType (ruleExpansions rule)) >>=
    (\exs -> if null exs
             then (Error $ "No compatible expansions found for type: " ++ show requiredType)
             else Good $ exs !! ((`mod` length exs) . head . stateChoices) state) >>=
    (\chosenExpansion -> Good (state, chosenExpansion))

chooseExpansion _ t = Error $ "Terminals cannot be expanded. Terminal: " ++ termName t


-- expands multiple terms, making sure each expansion operates with an updated state
expandTerms :: GeneratorState -> [Term] -> Possibly (GeneratorState, [SyntaxTree])
expandTerms initialState [] = Good $ (initialState, [])
expandTerms initialState (t:ts) = expandTerm initialState t >>=
                                  (\(nextState, tree) ->
                                       expandTerms nextState ts >>=
                                       (\(finalState, trees) -> Good $ (finalState, tree : trees)))


-- given expansions and a term, expands the term into a syntax tree until
-- there are no branches left to expand
expandTerm :: GeneratorState -> Term -> Possibly (GeneratorState, SyntaxTree)
expandTerm initialState (TerminalTerm name) = Good (initialState, Leaf name)
expandTerm initialState term = chooseExpansion initialState term >>=
                               (\(nextState, expansion) ->
                                    (expandTerms nextState . expansionTerms) expansion >>=
                               (\(nextState, children) ->
                                    inferType (stateTypes nextState) children (expansionTypeExpr expansion) >>=
                               (\inferredType ->
                                    Good (advance nextState, Branch term expansion inferredType children))))


-- -- builds a syntax tree from a start symbol
buildTree :: GeneratorState -> String -> Possibly SyntaxTree
buildTree state start = expandTerm state (NonterminalTerm "" start AnyType) >>=
                        (\(state, tree) -> Good tree)


-- -- builds a map associating names with types for a list of subtrees
syntaxTreeTypeMap :: [SyntaxTree] -> Map String Type
syntaxTreeTypeMap subtrees = fromList $ zip (map (termName . branchTerm) childrenBranches)
                             (map branchType childrenBranches)
                                 where
                                   childrenBranches = filter isBranch subtrees

-- -- evaluates a type expression in the context of subtrees
inferType :: [Type] -> [SyntaxTree] -> TypeExpression -> Possibly Type
inferType types subtrees = evalTypeExpression types $ syntaxTreeTypeMap subtrees
