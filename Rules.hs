{--
  Defines datatypes for expressing metalanguage rules
--}

import Control.Monad (mapM, filterM)
import Data.List (intersperse, find)
import Data.Map (Map, fromList, (!), member)
import Types
import AbstractTypeMatching

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
data Expansion = Expansion { expansionTerms :: [Term], expressionType :: TypeExpression }

instance Show Expansion where
    show (Expansion terms typeExpr) = "{ " ++ (concat . intersperse " " . map show) terms ++ " }::"
                                      ++ show typeExpr

-- gets all expansions compatible with the given type
compatibleExpansions :: [Type] -> Type -> [Expansion] -> Maybe [Expansion]
compatibleExpansions types t = filterM (isExpansionCompatible types t)


-- checks whether an expansion is compatible with the given type
isExpansionCompatible :: [Type] -> Type -> Expansion -> Maybe Bool
isExpansionCompatible types t expansion = evalTypeExpression types (expansionTypeMap expansion) (expressionType expansion) >>=
                                          (\expansionType -> Just $ expansionType `instanceOf` t)

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
evalTypeExpression :: [Type] -> Map String Type -> TypeExpression -> Maybe Type
evalTypeExpression types nameMap expr =
    case expr of
      (TypeIdentity cs) -> find (\t -> name t == cs) types

      (TypeOf name)     -> if member name nameMap
                           then Just $ nameMap ! name
                           else Nothing
                                            
      (TypeCoalesce exprA exprB) -> evalTypeExpression types nameMap exprA        >>=
                              (\typeA -> evalTypeExpression types nameMap exprB >>=
                                         (\typeB -> Just (typeA, typeB)))           >>=
                              (\(typeA, typeB) -> Just $ coalesce typeA typeB)


-- represents a grammar rule, e.g. expr -> expr + expr
data Rule = Rule { ruleName :: String, ruleExpansions :: [Expansion] }

-- returns rules with the same name as the given term's id, provided that term is a nonterminal
findRule :: [Rule] -> Term -> Maybe Rule
findRule rules (TerminalTerm _) = Nothing
findRule rules (NonterminalTerm _ id _) = find ((id ==) . ruleName) rules


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
                                   show term ++  " => " ++ show (expressionType ex) ++ " = " ++ show t ++ "\n"
                                   ++ identString ++ "\t- "
                                          ++ (concat . intersperse "\n\t- ")
                                                 (map (printSyntaxTree (ident + 1)) children)
                              where identString = replicate ident '\t'



-- given expansions and a term, expands the term into a syntax tree until
-- there are no branches left to expand
expandTerm :: [Type] -> [Rule] -> Term -> Maybe SyntaxTree
expandTerm _ rules (TerminalTerm name) = Just $ Leaf name
expandTerm types rules t@(NonterminalTerm name id requiredType) =
    findRule rules t >>=
    (\rule -> Just $ ruleExpansions rule) >>=
    (\expansions ->
         let chosenExpansion = compatibleExpansions types requiredType expansions >>=
                          (\exs -> if null exs then Nothing else Just $ head exs)
         in
         chosenExpansion >>=
         (mapM (expandTerm types rules)) . expansionTerms >>=
         (\children -> chosenExpansion >>=
                       (\ex -> let maybeInferredType = inferType types children (expressionType ex) in
                               case maybeInferredType of
                                 Nothing -> Nothing
                                 (Just inferredType) ->
                                     Just $ Branch t ex inferredType children))
    )


-- builds a syntax tree from a start symbol
buildTree :: [Type] -> [Rule] -> String -> Maybe SyntaxTree
buildTree types rules start = expandTerm types rules (NonterminalTerm "" start AnyType)


-- builds a map associating names with types for a list of subtrees
syntaxTreeTypeMap :: [SyntaxTree] -> Map String Type
syntaxTreeTypeMap subtrees = fromList $ zip (map (termName . branchTerm) childrenBranches)
                                        (map branchType childrenBranches)
                         where
                           childrenBranches = filter isBranch subtrees

-- evaluates a type expression in the context of subtrees
inferType :: [Type] -> [SyntaxTree] -> TypeExpression -> Maybe Type
inferType types subtrees = evalTypeExpression types $ syntaxTreeTypeMap subtrees
