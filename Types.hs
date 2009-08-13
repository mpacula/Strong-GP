{--
  Types.hs - type operations for the language-description metalanguage
--}

import Data.List

data Type = SimpleType  { name :: String }
          | DerivedType { name :: String, supertype :: Type}
          | ChainType { chain :: [Type] }
          | AnyType
            deriving (Eq)

instance Show Type where
    show = match $ simpleType (\t -> name t)                                    ==>
           derivedType (\t -> name t ++ " of " ++ name (supertype t))           ==>
           chainType   (\t -> concat (intersperse " -> " (map show (chain t)))) ==>
           anyType     (\t -> "Any")

simpleType :: (Type -> a) -> Type -> Maybe a
simpleType f t@(SimpleType _) = Just $ f t
simpleType _ _                = Nothing

derivedType :: (Type -> a) -> Type -> Maybe a
derivedType f t@(DerivedType _ _) = Just $ f t
derivedType _ _                   = Nothing

chainType :: (Type -> a) -> Type -> Maybe a
chainType f t@(ChainType _) = Just $ f t
chainType _ _               = Nothing

anyType :: (Type -> a) -> Type -> Maybe a
anyType f t@AnyType = Just $ f t
anyType _ _         = Nothing


type Matcher a b = a -> Maybe b

-- Matcher combinator. Given two matchers, creates a new matcher which calls
-- the first one and then the second one only if the result of the first is Nothing
(==>) :: Matcher a b -> Matcher a b -> Matcher a b
firstMatcher ==> secondMatcher = chainedMatcher where
    chainedMatcher arg =
        case fstResult of
          Nothing -> secondMatcher arg
          x@(Just _) -> fstResult
        where fstResult = firstMatcher arg

-- Calls a matcher on an argument, returning the unwrapped Just value if
-- such is returned. If the matcher returns Nothing, the call triggers
-- an error.
match :: Matcher a b -> a -> b
match m x = case result of
              Nothing -> error "Matching failed"
              (Just val) -> val
            where result = m x
                           

-- given a type, returns all supertypes including the queried type
supertypes :: Type -> [Type]
supertypes = match $
             anyType (\t -> [AnyType])         ==>
             simpleType (\t -> [t, AnyType])   ==>
             chainType (\t -> [t, AnyType])    ==>
             derivedType (\t -> t : supertypes (supertype t))


-- gets subtypes of the given type from a list of types
subtypes :: [Type] -> Type -> [Type]
subtypes ts t = filter (\x -> t `elem` supertypes x) ts


-- given 2 types, returns the closest common supertype, if any
coerce :: Type -> Type -> Type
coerce ta tb = head $ intersect (supertypes ta) (supertypes tb)
      

-- checks whether one type is an instance of another. Type A is an instance of type B
-- if B is a supertype of A
instanceOf :: Type -> Type -> Bool

(ChainType (t:[])) `instanceOf` (ChainType (y:[])) = t `instanceOf` y
(ChainType (t:ts)) `instanceOf` (ChainType (y:ys)) = 
    y `instanceOf` t  && (ChainType ts) `instanceOf` (ChainType ys)

ta `instanceOf` tb = tb `elem` supertypes ta

{--
  Test definitions
--}

{--
  Type hierarchy:
  * Number
    - Real
        - Rational-
           - Natural
           - Integer
        - Irrational
    - Complex
  * Number -> Number -> Rational
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