{--
  Types.hs - type operations for the language-description metalanguage
--}

module Types 
    (
      Type(..)
    , isPrimitive
    , isPolymorphic
    , isTypeVariable
    , isTypeCompatible
    ) where


import Data.List (intersperse, intersect)
import AbstractTypeMatching (Matcher, (==>))


data Type = PrimitiveType  { name :: String }
          | PolymorphicType { name :: String, vars :: [Type]}
          | TypeVariable { name :: String }
            deriving (Eq, Ord)


instance Show Type where
    show (PrimitiveType name)              = name
    show (PolymorphicType name vars)       = "(" ++ name ++ " " ++ (concat . intersperse " " . map show) vars ++ ")"
    show (TypeVariable name)               = name


isPrimitive, isPolymorphic, isTypeVariable :: Type -> Bool
isPrimitive (PrimitiveType _)       = True
isPrimitive _                       = False


isPolymorphic (PolymorphicType _ _) = True
isPolymorphic _ = False


isTypeVariable (TypeVariable _)     = True
isTypeVariable _                    = False


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
