import Test.QuickCheck
import Control.Monad
import Generator
import Types

{-
  TYPES
-}

capitalString :: Gen String
capitalString = do
  firstChar <- elements ['A' .. 'Z']
  rest <- arbitrary
  return $ firstChar : rest

arbitraryList :: (Arbitrary a) => (Int, Int) -> Gen [a]
arbitraryList range = do
  n <- choose range :: Gen Int
  mapM (\x -> arbitrary) (replicate n 0)

instance Arbitrary Type where
    coarbitrary = undefined
    arbitrary = oneof
                [ liftM PrimitiveType capitalString
                , do
                    varName <- elements ['a' .. 'z']
                    return $ TypeVariable (varName : [])
                , do
                    vars <- arbitraryList (1,2)
                    name <- capitalString
                    return $ PolymorphicType name vars
                ]

{-
  TERMINALS & NONTERMINALS
-}

instance Arbitrary Char where
    coarbitrary = undefined
    arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'])

instance Arbitrary Term where
    coarbitrary = undefined
    arbitrary =
        oneof
        [ liftM TerminalTerm arbitrary
        , liftM NonterminalTerm arbitrary
        ]


instance Arbitrary Expansion where
    coarbitrary = undefined
    arbitrary = do
      terms <- arbitraryList (0,10)
      t <- arbitrary
      return $ Expansion terms t


xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not (x && y)

prop_either_terminal x = (isTerminal x) `xor` (isNonterminal x)

prop_empty_has_nonterminals x = null (expansionTerms x) ==> not $ hasNonterminals x