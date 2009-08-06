{--
  Types.hs - type operations for the language-description metalanguage
--}

import Data.List

data Type = SimpleType  { name :: String }
          | DerivedType { name :: String, parent :: Type }
          | ListType    { list :: [Type] }
            deriving (Show, Eq)

-- given a type, returns all supertypes including the queried type
supertypes :: Type -> [Type]
supertypes t@(SimpleType _) = [t]
supertypes t@(DerivedType name parent) = t : supertypes parent
supertypes t@(ListType _) = [t]

-- given 2 types, returns the closest common supertype, if any
coalesce :: Type -> Type -> Maybe Type
coalesce ta tb
    | common == [] = Nothing
    | otherwise    = Just (head common)
    where 
      common = intersect (supertypes ta) (supertypes tb)
      

-- checks whether one type is an instance of another. Type A is an instance of type B
-- if B is a supertype of A
instanceOf :: Type -> Type -> Bool
ta `instanceOf` tb = tb `elem` supertypes ta

{--
  Test definitions
--}

{--
  Type hierarchy:
  * Number
    - Real
        - Rational
           - Natural
           - Integer
        - Irrational
    - Complex
--}
tNumber = SimpleType "Number"
tReal = DerivedType "Real" tNumber
tRational = DerivedType "Rational" tReal
tNatural = DerivedType "Natural" tRational
tInteger = DerivedType "Integer" tRational
tIrrational = DerivedType "Irrational" tReal
tComplex = DerivedType "Complex" tNumber
