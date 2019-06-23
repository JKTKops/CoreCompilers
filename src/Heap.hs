module Heap where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Addr = Integer
type Size = Integer

data Heap a = Heap Size [Addr] (Map Addr a)
  deriving (Eq, Show, Read)

empty :: Heap a
empty = Heap 0 [1..] Map.empty

allocate :: Heap a -> a -> (Heap a, Addr)
allocate (Heap size (next:free), bdgs) x = Heap (size+1) free (Map.insert next x bdgs)

update :: Heap a -> Addr -> a -> Heap a
update (Heap size free bdgs) addr x = Heap size free (Map.insert addr x bdgs)

free :: Heap a -> Addr -> Heap a
free (Heap size free bdgs) addr = Heap (size - 1) (a : free) (Map.delete addr bdgs)

deref :: Heap a -> Addr -> a
deref (Heap size free bdgs) addr = bdgs `Map.!` addr

(*!) :: Heap a -> Addr -> a
(*!) = deref

addresses :: Heap a -> [Addr]
addresses (Heap _ _ bdgs) = Map.keys bdgs

size :: Heap a -> Size
size (Heap size _ _) = size

instance Functor Heap where
    fmap f (Heap _ _ bdgs) = f <$> bdgs
