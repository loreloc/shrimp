{-# LANGUAGE LambdaCase #-}

module Shrimp.Parser where

import Shrimp.Exception
  ( Exception (EmptyProgram),
    Result (Error, Ok),
  )
import Shrimp.SyntaxTree
  ( ArithmeticExpr (..),
    Block,
    BooleanExpr (..),
    Command (..),
    Header,
    Program,
    Variable (..),
  )
import Shrimp.Utils
  ( MonadAlternative (many, some, (<|>)),
    MonadPlus (plus, zero),
  )

-- | Define the parser type
newtype Parser a = Parser {unwrap :: String -> [(a, String)]}

-- | Define the functor instance
instance Functor Parser where
  fmap f p = Parser (\cs -> [(f a, cs') | (a, cs') <- unwrap p cs])

-- | Define the applicative instance
instance Applicative Parser where
  pure a = Parser (\cs -> [(a, cs)])
  p <*> q = Parser (\cs -> concat [unwrap (fmap a q) cs' | (a, cs') <- unwrap p cs])

-- | Define the monad instance
instance Monad Parser where
  return a = pure a
  p >>= f = Parser (\cs -> concat [unwrap (f a) cs' | (a, cs') <- unwrap p cs])

-- | Define the monad plus instance
instance MonadPlus Parser where
  zero = Parser (const [])
  p `plus` q = Parser (\cs -> unwrap p cs ++ unwrap q cs)

-- | Define the alternative monad instance
instance MonadAlternative Parser where
  (<|>) p q =
    Parser
      ( \cs -> case unwrap (p `plus` q) cs of
          [] -> []
          (x : _) -> [x]
      )

-- | Parse a string
parse :: String -> Result (Program, String)
parse cs = case unwrap program cs of
  [] -> Error EmptyProgram
  [(p, cs)] -> Ok (p, cs)

-- | Item function that consumes a character
item :: Parser Char
item = Parser (\case "" -> []; (c : cs) -> [(c, cs)])

-- | Conditional function that consume a character
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- item; if p c then return c else zero

-- | Parse token
token :: Parser a -> Parser a
token p = do space; v <- p; space; return v

-- | Parse spaces
space :: Parser String
space = many $ satisfy isSpace

-- | Parse an identifier
identifier :: Parser String
identifier = token $ some $ satisfy isLetter

-- | Parse a constant
constant :: Parser Int
constant = read <$> token (some $ satisfy isDigit)

-- | Parse a specific character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parse a keyword
keyword :: String -> Parser String
keyword cs = token $ word cs

-- | Parse a word
word :: String -> Parser String
word [c] = do char c; return [c]
word (c : cs) = do char c; word cs; return (c : cs)

-- | Parse a symbol
symbol :: Char -> Parser Char
symbol c = token $ char c

-- | Check whether a character is a space
isSpace :: Char -> Bool
isSpace c
  | c == ' ' = True
  | c == '\t' = True
  | c == '\r' = True
  | c == '\n' = True
  | otherwise = False

-- | Check whether a character is a digit
isDigit :: Char -> Bool
isDigit c = c `elem` ['0' .. '9']

-- | Check whether a character is a letter or an underscore
isLetter :: Char -> Bool
isLetter c
  | c `elem` ['a' .. 'z'] = True
  | c `elem` ['A' .. 'Z'] = True
  | c == '_' = True
  | otherwise = False

-- | Parse a program
program :: Parser Program
program = do h <- header; keyword "shrimp"; b <- block; return (h, b)

-- | Parse a command block
block :: Parser Block
block = many command

-- | Parse a header
header :: Parser Header
header = many variable

-- | Parase a variable declearation
variable :: Parser Variable
variable = do
  keyword "let"
  d <- identifier
  keyword "as"
  do
    keyword "int"
    symbol ';'
    return (IntegerDecl d)
    <|> do
      keyword "bool"
      symbol ';'
      return (BooleanDecl d)
    <|> do
      keyword "array"
      symbol '['
      n <- constant
      symbol ']'
      symbol ';'
      return (ArrayDecl d n)

-- | Parse a command
command :: Parser Command
command = assignment <|> branch <|> loop <|> skip

-- | Parse an assignment command
assignment :: Parser Command
assignment = do
  d <- identifier
  do
    symbol '='
    a <- arithmeticExpr
    symbol ';'
    return (ArithmeticAssignment d a)
    <|> do
      symbol '='
      b <- booleanExpr
      symbol ';'
      return (BooleanAssignment d b)
    <|> do
      symbol '['
      k <- arithmeticExpr
      symbol ']'
      symbol '='
      a <- arithmeticExpr
      symbol ';'
      return (ArrayAssignment d k a)

-- | Parse a branch command
branch :: Parser Command
branch = do
  keyword "if"
  symbol '('
  b <- booleanExpr
  symbol ')'
  keyword "then"
  c1 <- block
  do
    keyword "else"
    c2 <- block
    keyword "end if"
    symbol ';'
    return (Branch b c1 c2)
    <|> do
      keyword "end if"
      symbol ';'
      return (Branch b c1 [Skip])

-- | Parse a loop command
loop :: Parser Command
loop = do
  keyword "while"
  symbol '('
  b <- booleanExpr
  symbol ')'
  keyword "do"
  c <- block
  keyword "end while"
  symbol ';'
  return (Loop b c)

-- | Parse a skip command
skip :: Parser Command
skip = do
  keyword "skip"
  symbol ';'
  return Skip

-- | Parse an arithmetic expression
arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr = do
  a <- arithmeticTerm
  do symbol '+'; Add a <$> arithmeticExpr
    <|> do symbol '-'; Sub a <$> arithmeticExpr
    <|> do return a

-- | Parse an arithmetic term
arithmeticTerm :: Parser ArithmeticExpr
arithmeticTerm = do
  a <- arithmeticFactor
  do symbol '*'; Mul a <$> arithmeticTerm
    <|> do symbol '/'; Div a <$> arithmeticTerm
    <|> do symbol '%'; Mod a <$> arithmeticTerm
    <|> do return a

-- | Parse an arithmetic factor
arithmeticFactor :: Parser ArithmeticExpr
arithmeticFactor =
  do Constant <$> constant
    <|> do
      d <- identifier
      do
        symbol '['
        k <- arithmeticExpr
        symbol ']'
        return (ArrayVar d k)
        <|> return (IntegerVar d)
    <|> do symbol '-'; Neg <$> arithmeticExpr
    <|> do symbol '('; a <- arithmeticExpr; symbol ')'; return a

-- | Parse a boolean expression
booleanExpr :: Parser BooleanExpr
booleanExpr = do
  b <- booleanTerm
  do keyword "or"; Or b <$> booleanExpr
    <|> do return b

-- | Parse a boolean term
booleanTerm :: Parser BooleanExpr
booleanTerm = do
  b <- booleanFactor
  do keyword "and"; And b <$> booleanTerm
    <|> do return b

-- | Parse a boolean factor
booleanFactor :: Parser BooleanExpr
booleanFactor =
  do keyword "true"; return (Truth True)
    <|> do keyword "false"; return (Truth False)
    <|> do keyword "not"; Not <$> booleanExpr
    <|> do symbol '('; b <- booleanExpr; symbol ')'; return b
    <|> do
      a <- arithmeticExpr
      do keyword "eq"; Equal a <$> arithmeticExpr
        <|> do keyword "neq"; NotEqual a <$> arithmeticExpr
        <|> do keyword "lt"; Less a <$> arithmeticExpr
        <|> do keyword "gt"; Greater a <$> arithmeticExpr
        <|> do keyword "leq"; LessEqual a <$> arithmeticExpr
        <|> do keyword "geq"; GreaterEqual a <$> arithmeticExpr
    <|> do BooleanVar <$> identifier
