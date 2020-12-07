module Shrimp.Parser where

import Shrimp.Grammar
  ( ArithmeticExpr (Add, Constant, Identifier, Mul, Sub),
    Block,
    BooleanExpr (And, Boolean, Equal, LessEqual, Not, Or),
    Command (Assignment, Branch, Loop, Skip),
  )

-- | Define the parser type
newtype Parser a = Parser (String -> [(a, String)])

-- | Define the parser unwrap function
unwrap :: Parser a -> (String -> [(a, String)])
unwrap (Parser p) = p

-- | Define an augment monad class
class Monad m => AugmentMonad m where
  zero :: m a
  (<|>) :: m a -> m a -> m a
  many :: m a -> m [a]
  some :: m a -> m [a]
  many p = some p <|> pure []
  some p = (:) <$> p <*> many p

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
  p >>= f =
    Parser
      ( \cs -> case unwrap p cs of
          [] -> []
          [(a, cs')] -> unwrap (f a) cs'
      )

-- | Define the augment monad instance
instance AugmentMonad Parser where
  zero = Parser (const [])
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
  let p = Assignment d a
  symbol ';'
  return p

-- | Parse a branch command
branch :: Parser Command
branch = do
  keyword "if"
  symbol '('
  b <- booleanExpr
  symbol ')'
  keyword "then"
  c1 <- block
  keyword "else"
  c2 <- block
  keyword "end if"
  let p = Branch b c1 c2
  symbol ';'
  return p

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
  return Skip

-- | Parse an arithmetic expression
arithmeticExpr :: Parser ArithmeticExpr
arithmeticExpr = ap <|> sp <|> arithmeticTerm
  where
    ap = do
      a <- arithmeticTerm
      symbol '+'
      Add a <$> arithmeticExpr
    sp = do
      a <- arithmeticTerm
      symbol '-'
      Sub a <$> arithmeticExpr

-- | Parse an arithmetic term
arithmeticTerm :: Parser ArithmeticExpr
arithmeticTerm = mp <|> arithmeticFactor
  where
    mp = do
      a <- arithmeticFactor
      symbol '*'
      Mul a <$> arithmeticTerm

-- | Parse an arithmetic factor
arithmeticFactor :: Parser ArithmeticExpr
arithmeticFactor = cp <|> ip <|> nested
  where
    cp = Constant <$> constant
    ip = Identifier <$> identifier
    nested = do
      symbol '('
      ep <- arithmeticExpr
      symbol ')'
      return ep

-- | Parse a boolean expression
booleanExpr :: Parser BooleanExpr
booleanExpr = op <|> lp <|> ep <|> booleanTerm
  where
    op = do
      b <- booleanTerm
      keyword "or"
      Or b <$> booleanExpr
    lp = do
      a <- arithmeticExpr
      keyword "leq"
      LessEqual a <$> arithmeticExpr
    ep = do
      a <- arithmeticExpr
      keyword "eq"
      Equal a <$> arithmeticExpr

-- | Parse a boolean term
booleanTerm :: Parser BooleanExpr
booleanTerm = ap <|> booleanFactor
  where
    ap = do
      b <- booleanFactor
      keyword "and"
      And b <$> booleanTerm

-- | Parse a boolean factor
booleanFactor :: Parser BooleanExpr
booleanFactor = tp <|> fp <|> np <|> nested
  where
    tp = do
      keyword "true"
      return (Boolean True)
    fp = do
      keyword "false"
      return (Boolean False)
    np = do
      keyword "not"
      Not <$> booleanExpr
    nested = do
      symbol '('
      ep <- booleanExpr
      symbol ')'
      return ep
