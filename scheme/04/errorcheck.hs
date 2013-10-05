module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = concat [message, ": ", varname]
showError (BadSpecialForm message form) = concat [message, ": ", show form]
showError (NotFunction message func) = concat [message, ": ", show func]
showError (NumArgs expected found) = concat ["Expected ", show expected,
                                            " args: found values ",
                                            unwordsList found]
showError (TypeMismatch expected found) = concat ["Invalid type: expected ",
                                                  expected, ", found ",
                                                  show found]
showError (Parser parseErr) = concat ["Parse error at ", show parseErr]

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError :: ThrowsError String -> ThrowsError String
trapError action = catchError action (return . show)

extractError :: ThrowsError a -> a
extractError (Right val) = val

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

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String s) = let parsed = reads s in
                       if null parsed
                       then throwError $ TypeMismatch "number" $ String s
                       else return . fst . head $ parsed
unpackNum (List [x]) = unpackNum x
unpackNum notNum = throwError . TypeMismatch "number" $ notNum

numericBinOp :: (Integer -> Integer -> Integer) -> [LispVal]
                -> ThrowsError LispVal
numericBinOp op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackNum params >>= return . Number . foldl1 op

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinOp (+)),
              ("-",         numericBinOp (-)),
              ("*",         numericBinOp (*)),
              ("/",         numericBinOp div),
              ("mod",       numericBinOp mod),
              ("quotient",  numericBinOp quot),
              ("remainder", numericBinOp rem)]

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
                  (throwError $ NotFunction
                   "Unrecognized primitive function args" func)
                  ($ args) $ lookup func primitives

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> throwError . Parser $ err
  Right val -> return val

main :: IO ()
main = do
  args <- getArgs
  evaled <- return . liftM show $ readExpr (args !! 0) >>= eval
  putStrLn . extractError . trapError $ evaled
