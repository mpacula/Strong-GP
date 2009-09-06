{-
  Defines datatypes and functions for manipulating symbol tables: hierarchical maps from symbols
  to values.
-}

module SymbolTable
    (
    ) where


import Types (Type)
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
                              , reverseBindings  :: Map TableValue [Name]
                              , parent           :: SymbolTable
                              }
                 | RootTable


instance Show SymbolTable where
    show RootTable                = "[Root]"
    show table@(InnerTable _ _ _) = parentString ++ unlines ["", "", "^", "|", "", "[", myString, "]"]
                                    where showBinding (key, value) = show key ++ " -> " ++ (show . valueType) value ++ "\n"
                                          parentString = show $ parent table
                                          myString     = concat . (map showBinding) $ (assocs . bindings) table


-- creates a new clean SymbolTable that's a child of the given one
mkChild :: SymbolTable -> SymbolTable
mkChild RootTable = InnerTable { parent = RootTable
                               , bindings = empty
                               , reverseBindings = empty
                               }
mkChild table = table { parent = table
                      , bindings = empty
                      , reverseBindings = empty
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
                          , reverseBindings = insertIntoList value name $ reverseBindings table
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
hasBinding name = (member name) . bindings


-- checks whether the given value is bound to at least one name in a symbol table
hasValue :: Type -> SymbolTable -> Bool
hasValue t = (member (TableValue t)) . reverseBindings


-- gets all names the given value is bound to in a symbol table
getNames :: Type -> SymbolTable -> [Name]
getNames t table = if hasValue t table
                   then (reverseBindings table) ! (TableValue t)
                   else []
