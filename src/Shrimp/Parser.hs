module Shrimp.Parser where

-- | Define the parser type
newtype Parser a = Parser (String -> [(a, String)])

-- | Parse `unwrapping` function
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

-- | Define an augment monad class
class Monad m => AugmentMonad m where
  zero :: m a
  plus :: m a -> m a -> m a

-- | Define the functor instance
instance Functor Parser where
  fmap f p = Parser (\cs -> [(f a, cs') | (a, cs') <- parse p cs])

-- | Define the applicative instance
instance Applicative Parser where
  pure v = Parser (\cs -> [(v, cs)])
  (<*>) (Parser f) p =
    Parser
      ( \cs ->
          concat
            [ [(g a, cs'') | (g, cs'') <- f cs'] | (a, cs') <- parse p cs
            ]
      )

-- | Define the monad instance
instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])

-- | Define the augment monad instance
instance AugmentMonad Parser where
  zero = Parser (const [])
  plus p q = Parser (\cs -> parse p cs ++ parse q cs)

-- | Deterministic addition of parsers
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) p q =
  Parser
    ( \cs -> case parse (p `plus` q) cs of
        [] -> []
        (x : _) -> [x]
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

-- | Many combinator that repeately applicate a parser
many :: Parser a -> Parser [a]
many p = do a <- p; as <- many p; return (a : as)

-- | Chain combinator (left-associative)
chain :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chain p op x = do a <- p; rest a <|> return x
  where
    rest a = do f <- op; b <- p; rest (f a b) <|> return a

-- | Parse spaces
space :: Parser String
space = many $ satisfy isSpace

-- | Parse an identifier
identifier :: Parser String
identifier = many $ satisfy isLetter

-- | Parse a constant
constant :: Parser Int
constant = fmap (\s -> read s :: Int) (many $ satisfy isDigit)

-- | Parse a specific character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parse a keyword
keyword :: String -> Parser String
keyword [c] = do char c; return [c]
keyword (c : cs) = do char c; keyword cs; return (c : cs)

-- | Apply a parser, removing leading space
apply :: Parser a -> String -> [(a, String)]
apply p = parse (do space; p)

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
