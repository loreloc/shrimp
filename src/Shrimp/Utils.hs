module Shrimp.Utils where

-- | Alternative class
class (Applicative f) => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  some v = liftA2 (:) v (some v <|> pure [])
  many :: f a -> f [a]
  many v = liftA2 (:) v (many v) <|> pure []

-- | Binary applicative lift
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- | Binary monadic sequencing
seqM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
seqM2 f x y = join $ liftA2 f x y

-- | Monadic join
join :: (Monad m) => m (m a) -> m a
join m = m >>= id
