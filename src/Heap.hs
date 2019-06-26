module Heap where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

type Addr = Integer
type Size = Integer

data Heap a = Heap Size [Addr] (Map Addr a)
  deriving Eq

instance Show a => Show (Heap a) where
    show (Heap size _ assocs) = "Heap (size = " ++ show size ++ ") " ++ show assocs

empty :: Heap a
empty = Heap 0 [1..] Map.empty

allocate :: Heap a -> a -> (Heap a, Addr)
allocate (Heap size (next:free) bdgs) x =
    (Heap (size + 1) free (Map.insert next x bdgs), next)

update :: Heap a -> Addr -> a -> Heap a
update (Heap size free bdgs) addr x = Heap size free (Map.insert addr x bdgs)

free :: Heap a -> Addr -> Heap a
free (Heap size free bdgs) addr = Heap (size - 1) (addr : free) (Map.delete addr bdgs)

deref :: Heap a -> Addr -> a
deref (Heap size free bdgs) addr = bdgs ! addr

(*!) :: Heap a -> Addr -> a
(*!) = deref

addresses :: Heap a -> [Addr]
addresses (Heap _ _ bdgs) = Map.keys bdgs

bindings :: Heap a -> Map Addr a
bindings (Heap _ _ bdgs) = bdgs

size :: Heap a -> Size
size (Heap size _ _) = size

instance Functor Heap where
    fmap f (Heap size free bdgs) = Heap size free (f <$> bdgs)

instance Foldable Heap where
    foldMap f (Heap _ _ bdgs) = foldMap f bdgs

instance Traversable Heap where
    traverse f (Heap size free bdgs) = Heap size free <$> traverse f bdgs
    -- traverse f bdgs :: Applicative f => f (Map Addr a)
