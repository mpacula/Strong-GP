{--
  Defines datatypes for expressing metalanguage rules
--}

import Control.Applicative
import Data.List
import Types
import AbstractTypeMatching

-- represents a term of the form name@id::type, e.g. fst_expr@expr::AnyType
data Term = NonterminalTerm { termName :: String, termId :: String, termRequiredType :: Type  } 
          | TerminalTerm    { termValue :: String }

-- represents one possible expansion of a nonterminal
data Expansion = Expansion { expansionTerms :: [Term], expressionType :: TypeExpression }

-- represents a type expression, e.g. ::String or ::coalesce Number Integer
data TypeExpression = TypeIdentity String
                    | TypeCoalesce String String

-- evaluates a type expression given a list of types
evalTypeExpression :: [Type] -> TypeExpression -> Maybe Type
evalTypeExpression types expr = case expr of
                                  (TypeIdentity cs) -> find (\t -> name t == cs) types
                                  (TypeCoalesce cs vs) -> evalTypeExpression types (TypeIdentity cs) >>=
                                                          (\typeA -> evalTypeExpression types (TypeIdentity vs) >>=
                                                                     (\typeB -> Just (typeA, typeB))) >>=
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

rule1 = Rule "expr" [Expansion
                     [
                      (NonterminalTerm "a" "num" tNumber),
                      (TerminalTerm "+"),
                      (NonterminalTerm "b" "num" tNumber)
                     ]
                     (TypeCoalesce "a" "b")]

rule2 = Rule "num" [Expansion [TerminalTerm "1"] (TypeIdentity "Number")]


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