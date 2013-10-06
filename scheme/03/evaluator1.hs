module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List . sepBy parseExpr $ spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  init <- endBy parseExpr spaces
  char '.'
  spaces
  last <- parseExpr
  return $ DottedList init last

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  e <- parseExpr
  return $ List [Atom "quote", e]

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do
          char '('
          x <- (try parseList) <|> parseDottedList
          char ')'
          return x

showVal :: LispVal -> String
showVal (Atom atom) = atom
showVal (Number n) = show n
showVal (String contents) = concat ["\"", contents, "\""]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List list) = concat ["(", unwordsList list, ")"]
showVal (DottedList init last) =
  concat ["(", unwordsList init, " . ", showVal last, ")"]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> "No match: " ++ show err
  Right val -> "Found " ++ show val

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
