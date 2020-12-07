module Shrimp.Parser where

-- | Define the parser type
newtype Parser a = Parser (String -> [(a, String)])

-- | Parse `unwrapping` function
parse :: Parser a -> (String -> [(a, String)])
parse (Parser p) = p

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

-- | Define an `augmented` monad class
class Monad m => AugmentedMonad m where
  zero :: m a
  plus :: m a -> m a -> m a

-- | Define the `augmented` monad instance
instance AugmentedMonad Parser where
  zero = Parser (const [])
  plus p q = Parser (\cs -> parse p cs ++ parse q cs)

-- | Deterministic addition of parsers
($$) :: Parser a -> Parser a -> Parser a
($$) p q =
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
sat :: (Char -> Bool) -> Parser Char
sat p = do c <- item; if p c then return c else zero

-- | Many combinator that repeately applicate a parser
many :: Parser a -> Parser [a]
many p = do a <- p; as <- many p; return (a : as)

-- | Parse spaces
space :: Parser String
space = many (sat isSpace)

-- | Parse a specific character
char :: Char -> Parser Char
char c = sat (c ==)

-- | Parse string
string :: String -> Parser String
string "" = return ""
string (c : cs) = do char c; string cs; return (c : cs)

-- | Parse a token
token :: Parser a -> Parser a
token p = do a <- p; space; return a

-- | Parse a symbolic token
symbol :: String -> Parser String
symbol cs = token (string cs)

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
