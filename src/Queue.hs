module Queue
  ( Queue  -- | Queue data structure.
  , emptyQ   -- | Creates empty Queue.
  , isEmptyQ -- | Checks whether Queue is empty.
  , addQ     -- | Adds element to Queue.
  , remQ     -- | Removes element from the Queue, returns tuple consisting of removed element and remainings of the Queue.
  ) where

--interface (signature, contract)
emptyQ :: Queue a
isEmptyQ :: Queue a -> Bool
addQ :: a -> Queue a -> Queue a
remQ :: Queue a -> (a, Queue a)

--implementation
newtype Queue a = MkQueue [a] deriving Show

emptyQ = MkQueue []
isEmptyQ (MkQueue q) = null q
addQ x (MkQueue q) = MkQueue (q ++ [x])
remQ (MkQueue (q:qq)) = (q,MkQueue qq)