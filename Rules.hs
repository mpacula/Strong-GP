{--
  Defines datatypes for expressing metalanguage rules
--}

import Control.Applicative
import Control.Monad
import Data.List
import Types
import AbstractTypeMatching

-- represents a term of the form name@id::type, e.g. fst_expr@expr::AnyType
data Term = NonterminalTerm { termName :: String, termId :: String, termRequiredType :: Type } 
          | TerminalTerm    { termName :: String }

instance Show Term where
    show (NonterminalTerm name id requiredType) = name ++ "@" ++ id ++ "::(" ++ show requiredType ++ ")"
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
isExpansionCompatible types t expansion = evalTypeExpression types expansion (expressionType expansion) >>=
                                          (\expansionType -> Just $ expansionType `instanceOf` t)

-- represents a type expression, e.g. ::String or ::coalesce Number Integer
data TypeExpression = TypeIdentity String                        -- e.g. ::Number
                    | TypeOf String                              -- e.g, ::typeOf my_expr 
                    | TypeCoalesce TypeExpression TypeExpression -- e.g  ::coalesce (typeOf lhs) (typeOf rhs)
                      deriving (Show)

-- evaluates a type expression for the given expansion
evalTypeExpression :: [Type] -> Expansion -> TypeExpression -> Maybe Type
evalTypeExpression types expansion expr =
    case expr of
      (TypeIdentity cs) -> find (\t -> name t == cs) types

      (TypeOf name)     -> find (\term -> name == termName term) (expansionTerms expansion) >>=
                           (\term -> Just $ termRequiredType term)
                                            
      (TypeCoalesce exprA exprB) -> evalTypeExpression types expansion exprA        >>=
                              (\typeA -> evalTypeExpression types expansion exprB >>=
                                         (\typeB -> Just (typeA, typeB)))           >>=
                              (\(typeA, typeB) -> Just $ coalesce typeA typeB)


-- represents a grammar rule, e.g. expr -> expr + expr
data Rule = Rule { ruleName :: String, ruleExpansions :: [Expansion] }


{--
  Test grammar:
  Rule 1: expr -> {a@num::Number "+" b@num::Number}::coalesce a b
  Rule 2: num -> { "1" } :: Integer
                 { "2" } :: Integer
--}

tNumber = SimpleType "Number"
tReal = DerivedType "Real" tNumber
tRational = DerivedType "Rational" tReal
tNatural = DerivedType "Natural" tRational
tInteger = DerivedType "Integer" tRational
tIrrational = DerivedType "Irrational" tReal
tComplex = DerivedType "Complex" tNumber
tNumberNumberRational = ChainType [tNumber, tNumber, tRational]
allTypes = [tNumber, tReal, tRational, tNatural, tInteger, tIrrational, tComplex, tNumberNumberRational]

expansion1 = Expansion
                     [
                      (NonterminalTerm "a" "num" tNumber),
                      (TerminalTerm "+"),
                      (NonterminalTerm "b" "num" tNumber)
                     ]
             (TypeCoalesce (TypeOf "a") (TypeOf "b"))

rule1 = Rule "expr" [expansion1]

rule2 = Rule "num" [Expansion [TerminalTerm "1"] (TypeIdentity "Integer"),
                    Expansion [TerminalTerm "2"] (TypeIdentity "Integer")]


-- represents syntax trees generated from the meta grammar
data SyntaxTree = Branch { branchRule :: Rule, 
                           branchExpansion :: Expansion, 
                           branchType :: Type, 
                           children :: [SyntaxTree] }
                | Leaf   { leafValue :: String,
                           leafType :: Type }


-- evaluates a type expression in a syntax tree's context
--evalTypeExpression :: [Type] -> TypeExpression -> SyntaxTree -> Type
--evalTypeExpression types (TypeIdentity name) tree = undefined
--

-- gives the most specific type of an expansion that can be inferred without performing
-- any actual substitution
estimateType :: [Type] -> Expansion -> Type
estimateType = undefined