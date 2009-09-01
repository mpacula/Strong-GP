{-
  Parses grammars
-}


module GrammarParser
    (
      parseGrammar
    ) where


import Text.ParserCombinators.Parsec
import Generator (Term(..), Expansion(..))
import Types (Type(..))
import Possibly (Possibly(..))
import Utils (partitionEithers)


skipWhitespace :: Parser ()
skipWhitespace = skipMany (space <|> tab)


parseTypeVariable :: Parser Type
parseTypeVariable = do firstChar <- lower
                       rest <- many letter
                       return $ TypeVariable (firstChar : rest)


parsePrimitiveType :: Parser Type
parsePrimitiveType = do firstChar <- upper
                        rest <- many letter
                        return $ PrimitiveType (firstChar : rest)


parsePolymorphicType :: Parser Type
parsePolymorphicType = do char '['
                          name <- many1 letter
                          vars <- many (skipWhitespace >> parseType)
                          skipWhitespace
                          char ']'
                          return $ PolymorphicType name vars


parseType :: Parser Type
parseType = do result <- (parsePolymorphicType <|> parseTypeVariable <|> parsePrimitiveType)
               skipWhitespace
               return result
                         


parseTerminal :: Parser Term
parseTerminal = do val <- many1 (noneOf ": \n\t")
                   skipWhitespace
                   return $ TerminalTerm val


parseNonterminal :: Parser Term
parseNonterminal = do char '<'
                      skipWhitespace
                      t <- parseType
                      skipWhitespace
                      char '>'
                      skipWhitespace
                      return $ NonterminalTerm t


parseTerm :: Parser Term
parseTerm = do parseNonterminal <|> parseTerminal


parseExpansion :: Parser Expansion
parseExpansion = do terms <- many parseTerm
                    skipWhitespace
                    string "::"
                    skipWhitespace
                    requiredType <- parseType
                    skipWhitespace
                    return $ Expansion terms requiredType
                                        

parseGrammar :: String -> Possibly [Expansion]
parseGrammar input = case partitionEithers $ map (parse parseExpansion "Expansion") (lines input) of
                       ([], rights) -> Good  $ map (\(Right expansion) -> expansion) rights
                       (lefts, _)   -> Error $ (unlines . map (\(Left err) -> show err ++ "\n")) lefts


main :: IO ()
main = do contents <- readFile "/home/maciej/dev/improve/languages/math.grm"
          case parseGrammar contents of
            Good expansions -> putStrLn $ show expansions
            Error msg       -> putStrLn msg
