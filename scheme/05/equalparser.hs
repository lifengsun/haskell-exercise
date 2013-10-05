{-# OPTIONS_GHC -XExistentialQuantification #-}

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


boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool)
             -> [LispVal] -> ThrowsError LispVal
boolBinOp unpacker op args = if length args /= 2
                             then throwError . NumArgs 2 $ args
                             else do
                               left  <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool (op left right)

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number n) = return . show $ n
unpackStr (Bool b) = return . show $ b
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

strBoolBinop  = boolBinOp unpackStr
numBoolBinOp  = boolBinOp unpackNum
boolBoolBinOp = boolBinOp unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList (_:x:xs) y] = return $ DottedList (x:xs) y
cdr [DottedList [x] y] = return  y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ [x] ++ xs
cons [x, DottedList xs y] = return $ DottedList ([x] ++ xs) y
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)] = return $ Bool $ x == y
eqv [(Number x), (Number y)] = return $ Bool $ x == y
eqv [(String x), (String y)] = return $ Bool $ x == y
eqv [(Atom x), (Atom y)] = return $ Bool $ x == y
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (x:xs), List (y:ys)]
eqv [(List xs), (List ys)] = return . Bool $
                             length xs == length ys
                             && (and . map eqvPair . zip xs $ ys)
  where eqvPair (x, y) = case eqv [x, y] of
          Left err -> False
          Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals mx my (AnyUnpacker unpacker) = do x <- unpacker mx
                                               y <- unpacker my
                                               return $ x == y
                                            `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [mx, my] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals mx my)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr,
                      AnyUnpacker unpackBool]
  eqvEquals <- eqv [mx, my]
  return $ Bool $ (primitiveEquals || let Bool x = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+",         numericBinOp  (+)),
              ("-",         numericBinOp  (-)),
              ("*",         numericBinOp  (*)),
              ("/",         numericBinOp  div),
              ("mod",       numericBinOp  mod),
              ("quotient",  numericBinOp  quot),
              ("remainder", numericBinOp  rem),
              ("=",         numBoolBinOp  (==)),
              ("<",         numBoolBinOp  (<)),
              (">",         numBoolBinOp  (>)),
              ("/=",        numBoolBinOp  (/=)),
              (">=",        numBoolBinOp  (>=)),
              ("<=",        numBoolBinOp  (<=)),
              ("&&",        boolBoolBinOp (&&)),
              ("||",        boolBoolBinOp (||)),
              ("string=?",  strBoolBinop  (==)),
              ("string<?",  strBoolBinop  (<)),
              ("string<=?", strBoolBinop  (<=)),
              ("string>=?", strBoolBinop  (>=)),
              ("car",       car),
              ("cdr",       cdr),
              ("cons",      cons),
              ("eq?",       eqv),
              ("eqv?",      eqv),
              ("equal?",      equal)
             ]

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
eval (List [Atom "if", pred, conseq, alt]) = do
  b <- eval pred
  case b of
    Bool False -> eval alt
    otherwise -> eval conseq
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
