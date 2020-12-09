module Shrimp.Parser where

import Shrimp.Grammar
  ( ArithmeticExpr (Add, Constant, Div, Identifier, Mod, Mul, Neg, Sub),
    Block,
    BooleanExpr (And, Boolean, Equal, Greater, GreaterEqual, Less, LessEqual, Not, NotEqual, Or),
    Command (Assignment, Branch, Loop, Skip),
  )
import Shrimp.Utils (Alternative (empty, many, some, (<|>)))

-- | Define the parser type
newtype Parser a = Parser (String -> [(a, String)])

-- | Define the parser unwrap function
unwrap :: Parser a -> (String -> [(a, String)])
unwrap (Parser p) = p

-- | Define the functor instance
instance Functor Parser where
  fmap f p =
    Parser
      ( \cs -> case unwrap p cs of
          [] -> []
          [(a, cs')] -> [(f a, cs')]
      )

-- | Define the applicative instance
instance Applicative Parser where
  pure v = Parser (\cs -> [(v, cs)])
  (<*>) p q =
    Parser
      ( \cs -> case unwrap p cs of
          [] -> []
          [(a, cs')] -> unwrap (fmap a q) cs'
      )

-- | Define the monad instance
instance Monad Parser where
  (>>=) p f =
    Parser
      ( \cs -> case unwrap p cs of
          [] -> []
          [(a, cs')] -> unwrap (f a) cs'
      )

-- | Define the augment monad instance
instance Alternative Parser where
  empty = Parser (const [])
  (<|>) p q =
    Parser
      ( \cs -> case unwrap p cs of
          [] -> unwrap q cs
          ((a, cs) : _) -> [(a, cs)]
      )

-- | Item function that consumes a character
item :: Parser Char
item =
  Parser
    ( \cs -> case cs of
        "" -> []
        (c : cs) -> [(c, cs)]
    )

-- | Conditional function that consume a character
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do c <- item; if p c then return c else empty

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
keyword cs = token $ keyword' cs
  where
    keyword' [c] = do char c; return [c]
    keyword' (c : cs) = do char c; keyword' cs; return (c : cs)

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

-- | Parse a string
parse :: String -> (Block, String)
parse cs = case unwrap program cs of
  [] -> errorWithoutStackTrace "parsing error"
  [(b, cs)] -> (b, cs)

-- | Parse a program
program :: Parser Block
program = do keyword "shrimp"; block

-- | Parse a command
command :: Parser Command
command = assignment <|> branch <|> loop <|> skip

-- | Parse a command block
block :: Parser [Command]
block = many command

-- | Parse an assignment command
assignment :: Parser Command
assignment = do
  d <- identifier
  symbol '='
  a <- arithmeticExpr
  symbol ';'
  return (Assignment d a)

-- | Parse a branch command
branch :: Parser Command
branch =
  do
    keyword "if"
    symbol '('
    b <- booleanExpr
    symbol ')'
    keyword "then"
    c1 <- block
    keyword "else"
    c2 <- block
    keyword "end if"
    symbol ';'
    return (Branch b c1 c2)
  <|>
  do
    keyword "if"
    symbol '('
    b <- booleanExpr
    symbol ')'
    keyword "then"
    c <- block
    keyword "end if"
    symbol ';'
    return (Branch b c [Skip])

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
  let p = Loop b c
  symbol ';'
  return p

-- | Parse a skip command
skip :: Parser Command
skip = do
  keyword "skip"
  symbol ';'
  return Skip

-- | Parse an arithmetic expression
arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr =
  do a <- arithmeticTerm; symbol '+'; Add a <$> arithmeticExpr
  <|>
  do a <- arithmeticTerm; symbol '-'; Sub a <$> arithmeticExpr
  <|>
  do arithmeticTerm

-- | Parse an arithmetic term
arithmeticTerm :: Parser ArithmeticExpr
arithmeticTerm =
  do a <- arithmeticFactor; symbol '*'; Mul a <$> arithmeticTerm
  <|>
  do a <- arithmeticFactor; symbol '/'; Div a <$> arithmeticTerm
  <|>
  do a <- arithmeticFactor; symbol '%'; Mod a <$> arithmeticTerm
  <|>
  do arithmeticFactor

-- | Parse an arithmetic factor
arithmeticFactor :: Parser ArithmeticExpr
arithmeticFactor =
  do Constant <$> constant
  <|>
  do Identifier <$> identifier
  <|>
  do symbol '-'; Neg <$> arithmeticExpr
  <|>
  do symbol '('; a <- arithmeticExpr; symbol ')'; return a

-- | Parse a boolean expression
booleanExpr :: Parser BooleanExpr
booleanExpr =
  do b <- booleanTerm; keyword "or"; Or b <$> booleanExpr
  <|>
  do booleanTerm

-- | Parse a boolean term
booleanTerm :: Parser BooleanExpr
booleanTerm =
  do b <- booleanFactor; keyword "and"; And b <$> booleanTerm
  <|>
  do booleanFactor

-- | Parse a boolean factor
booleanFactor :: Parser BooleanExpr
booleanFactor =
  do keyword "true"; return (Boolean True)
  <|>
  do keyword "false"; return (Boolean False)
  <|>
  do keyword "not"; Not <$> booleanExpr
  <|>
  do a <- arithmeticExpr; keyword "eq"; Equal a <$> arithmeticExpr
  <|>
  do a <- arithmeticExpr; keyword "neq"; NotEqual a <$> arithmeticExpr
  <|>
  do a <- arithmeticExpr; keyword "lt"; Less a <$> arithmeticExpr
  <|>
  do a <- arithmeticExpr; keyword "gt"; Greater a <$> arithmeticExpr
  <|>
  do a <- arithmeticExpr; keyword "leq"; LessEqual a <$> arithmeticExpr
  <|>
  do a <- arithmeticExpr; keyword "geq"; GreaterEqual a <$> arithmeticExpr
  <|>
  do symbol '('; b <- booleanExpr; symbol ')'; return b
