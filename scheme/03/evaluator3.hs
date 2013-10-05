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
  concat ["(", unwordsList init, ".", showVal last]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String s) = let parsed = reads s in
                       if null parsed
                       then 0
                       else fst . head $ parsed
unpackNum (List [x]) = unpackNum x
unpackNum _ = 0

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinOp op = Number . foldl1 op . map unpackNum

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+",         numericBinOp (+)),
              ("-",         numericBinOp (-)),
              ("*",         numericBinOp (*)),
              ("/",         numericBinOp div),
              ("mod",       numericBinOp mod),
              ("quotient",  numericBinOp quot),
              ("remainder", numericBinOp rem)]

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func . map eval $ args

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String $ "No match: " ++ show err
  Right val -> val

main :: IO ()
main = do
  args <- getArgs
  putStrLn . show . eval . readExpr $ (args !! 0)
