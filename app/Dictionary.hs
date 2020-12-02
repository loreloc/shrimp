module Dictionary where

-- |Declare a Dictionary k v type
newtype Dictionary k v = Dictionary [(k, v)]
    deriving(Show)

-- |Empty dictionary
empty :: (Eq k) => Dictionary k v
empty = Dictionary []

-- |Check empty dictionary
isempty :: (Eq k) => Dictionary k v -> Bool
isempty (Dictionary []) = True
isempty _ = False

-- |Lookup from dictionary
search :: (Eq k) => Dictionary k v -> k -> Maybe v
search (Dictionary []) _ = Nothing
search (Dictionary ((h, v) : ps)) k =
    if k == h then Just v else search (Dictionary ps) k

-- |Insert into dictionary
insert :: (Eq k) => Dictionary k v -> k -> v -> Dictionary k v
insert (Dictionary []) k v = Dictionary [(k, v)]
insert (Dictionary ((h, u) : ps)) k v =
    if k == h then Dictionary ((h, v) : ps) else Dictionary ((h, u) : ds)
        where (Dictionary ds) = insert (Dictionary ps) k v
