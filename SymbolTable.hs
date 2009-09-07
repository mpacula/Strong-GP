{-
  Defines datatypes and functions for manipulating symbol tables: hierarchical maps from symbols
  to values.
-}


module SymbolTable
    (
    ) where


import Types (Type (..), isTypeCompatible)
import Data.List (intersperse)
import Data.Map (Map, fromList, member, insert, (!), assocs, empty)


-- values of this type are names that get bound in the symbol table
type Name = String


-- a value that names in a table bind to
data TableValue = TableValue { valueType :: Type
                             } deriving (Eq, Ord)


-- a symbol table: binds names to values
-- InnerTable: table that's not a root within the hierarchy
-- RootTable:  top level table with no bindings and no parent
data SymbolTable = InnerTable { bindings         :: Map Name TableValue
                              , parent           :: SymbolTable
                              }
                 | RootTable


instance Show SymbolTable where
    show RootTable              = "[Root]\n"
    show table@(InnerTable _ _) = parentString ++ unlines ["", "(^ parent)", "", "[", myString, "]"]
        where showBinding (key, value) = show key ++ " -> " ++ (show . valueType) value
              parentString             = show $ parent table
              myString                 = concat . intersperse "\n" . (map showBinding) $ (assocs . bindings) table


-- creates a new clean SymbolTable that's a child of the given one
mkChild :: SymbolTable -> SymbolTable
mkChild RootTable = InnerTable { parent = RootTable
                               , bindings = empty
                               }

mkChild table = table { parent = table
                      , bindings = empty
                      }

-- convenience method. Adds a new element to the list a key maps to,
-- or creates a new mapping if one doesn't already exists. The element is added
-- at the beginning of the list.
insertIntoList :: (Ord k) => k -> a -> Map k [a] -> Map k [a]
insertIntoList key val map = insert key (val:current) map
    where
      current = if member key map
                then map ! key
                else []


-- binds a name to a value in a symbol table
bind :: Name -> Type -> SymbolTable -> SymbolTable
bind name t table = table { bindings = insert name value $ bindings table
                          }
    where
      value = (TableValue t)


-- looks up a binding by name in a symbol table
lookupBinding :: Name -> SymbolTable -> Type
lookupBinding name RootTable = error $ "Binding not found for name: " ++ name
lookupBinding name table
    | hasBinding name table = valueType $ (bindings table) ! name
    | otherwise             = lookupBinding name $ parent table


-- checks whether a symbol table has a binding with the given name
hasBinding :: Name -> SymbolTable -> Bool
hasBinding name RootTable = False
hasBinding name table = or [ (member name) . bindings $ table
                           , hasBinding name . parent $ table
                           ]


-- Gets all names that bind a value type-compatible with the given type.
-- Definitions lower in the hierarchy shadow those higher up.
getCompatibleNames :: Type -> SymbolTable -> [Name]
getCompatibleNames requiredType table = getCompatibleNames' [] table
    where
      getCompatibleNames' :: [Name] -> SymbolTable -> [Name]
      getCompatibleNames' _          RootTable = []
      getCompatibleNames' shadowList table     =
          compatibleNames ++ getCompatibleNames' (shadowList ++ allNames) (parent table)
              where
                allAssocs        = assocs . bindings $ table
                visibleAssocs    = filter ((`notElem` shadowList) . fst) allAssocs
                compatibleAssocs = filter ((`isTypeCompatible` requiredType) . valueType . snd) visibleAssocs
                allNames         = map fst allAssocs
                compatibleNames  = map fst compatibleAssocs


{-
  DEBUG DEFINITIONS
-}


table1 = mkChild RootTable
table2 = bind "x" (PrimitiveType "Int") table1
table3 = bind "y" (PrimitiveType "String") table2

table4 = mkChild table3
table5 = bind "z" (PrimitiveType "Func") table4

table = bind "x2" (PolymorphicType "List" [TypeVariable "Int"]) table5
