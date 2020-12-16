module Shrimp.Utils where

-- | Monad plus class
class (Monad m) => MonadPlus m where
  zero :: m a
  plus :: m a -> m a -> m a

-- | Monad alternative class
class (MonadPlus m) => MonadAlternative m where
  (<|>) :: m a -> m a -> m a
  many :: m a -> m [a]
  many m = some m <|> return []
  some :: m a -> m [a]
  some m = liftA2 (:) m (many m)
  chain :: m a -> m (a -> a -> a) -> m a
  chain p o = do a <- p; rest a
    where
      rest a = (do f <- o; a' <- p; rest (f a a')) <|> return a

-- | Binary applicative lift
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- | Binary monadic sequencing
seqM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
seqM2 f x y = join $ liftA2 f x y

-- | Monadic join
join :: (Monad m) => m (m a) -> m a
join m = m >>= id
