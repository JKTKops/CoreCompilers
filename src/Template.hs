module Template where

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (mapAccumL)

import Language
import Heap (Heap, Addr, (*!))
import qualified Heap as Heap

import Pretty.Print

-- Types
data TiState = TiState { stack   :: TiStack
                       , dump    :: TiDump
                       , heap    :: TiHeap
                       , globals :: TiGlobals
                       , stats   :: TiStats
                       }
  deriving (Eq, Show)

type TiStack = [Addr]

data TiDump = DummyTiDump deriving (Eq, Show, Read)
initialTiDump = DummyTiDump

type TiHeap = Heap Node

data Node = NAp Addr Addr                   -- Application
          | NSupercomb Name [Name] CoreExpr -- Supercombinator
          | NNum Int                        -- A number
          | NInd Addr
          | NMarked Node
  deriving (Eq, Show, Read)

type TiGlobals = Map Name Addr

data TiStats = TiStats Integer deriving (Eq, Ord, Show, Read)

tiStatInitial :: TiStats
tiStatInitial = TiStats 0

tiStatIncSteps :: TiStats -> TiStats
tiStatIncSteps (TiStats s) = TiStats $ s + 1

tiStatGetSteps :: TiStats -> Integer
tiStatGetSteps (TiStats steps) = steps

mapStats :: (TiStats -> TiStats) -> TiState -> TiState
mapStats f state = state { stats = f $ stats state }

{- The compiler:
   Any compiler can be modeled as producing the initial state for
   a state transition machine as well as a 'bootstrap' which is the actual
   state transition machine. In the common case, the actual computer's
   machine code is the bootstrap but for functional languages we need
   a custom runtime.
-}

compile (!program) =
    TiState { stack   = initialStack
            , dump    = initialTiDump
            , heap    = initialHeap
            , globals = globals
            , stats   = tiStatInitial
            }
  where
    scDefs = program ++ preludeDefs ++ extraPreludeDefs
    (initialHeap, globals) = buildInitialHeap scDefs
    initialStack = [addressOfMain]
    addressOfMain = Map.findWithDefault (error "main is not defined") "main" globals

extraPreludeDefs :: [CoreScDefn]
extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, Map Name Addr)
buildInitialHeap scDefs = Map.fromList <$> mapAccumL allocateSc Heap.empty scDefs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
  where (heap', addr) = Heap.allocate heap (NSupercomb name args body)

eval :: TiState -> [TiState]
eval state = state : rest
  where
    rest | tiFinal state = []
         | otherwise     = eval nextState
    nextState = doAdmin $ step state

doAdmin :: TiState -> TiState
doAdmin state@(TiState _ _ heap _ stats) =
    let state' = if Heap.size heap > 10 then gc state else state
    in state' { stats = tiStatIncSteps stats }

tiFinal :: TiState -> Bool
tiFinal (TiState [soleAddr] _ heap _ _) = isDataNode $ heap *! soleAddr
tiFinal (TiState [] _ _ _ _) = error "Empty stack during evaluation!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TiState -> TiState
step state@(TiState stack _ heap _ _) = case heap *! head stack of
    NNum n -> numStep state n
    NAp a1 a2 -> apStep state a1 a2
    NSupercomb sc args body -> scStep state sc args body
    NInd addr -> state { stack = addr : tail stack }

numStep :: TiState -> Int -> TiState
numStep _ _ = error "Number applied as a function!"

apStep :: TiState -> Addr -> Addr -> TiState
apStep state a1 _ = state { stack = a1 : stack state }

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep state@(TiState stack _ heap globals _) name args body =
    state { stack = stack', heap = heap' }
  where
    stack' = drop (length args) stack
    root = head stack'
    heap' = instantiateAndUpdate body root heap env
    env = argBindings `Map.union` globals
    argBindings = Map.fromList $ zip args $ getArgPtrs heap stack

getArgPtrs :: TiHeap -> TiStack -> [Addr]
getArgPtrs heap (sc : stack) = map getArgPtr stack
  where getArgPtr addr = let NAp _ arg = heap *! addr in arg

instantiate :: CoreExpr -> TiHeap -> Map Name Addr -> (TiHeap, Addr)
instantiate (ENum n) heap env = Heap.allocate heap (NNum n)
instantiate (EAp e1 e2) heap env = Heap.allocate heap'' (NAp a1 a2)
  where (heap' , a1) = instantiate e1 heap  env
        (heap'', a2) = instantiate e2 heap' env
instantiate (EVar v) heap env =
    ( heap
    , Map.findWithDefault (error $ "Unbound identifier: " ++ v) v env
    )
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case expressions"

instantiateConstr :: Int -> Int -> TiHeap -> Map Name Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env = error "Can't instantiate constructors yet"

instantiateLet :: IsRec
               -> [(Name, CoreExpr)]
               -> CoreExpr
               -> TiHeap
               -> Map Name Addr
               -> (TiHeap, Addr)
instantiateLet isrec defs body heap env =
    instantiate body heap' env'
  where
    (heap', bindings) = mapAccumL instantiateRhs heap defs
    env' = (Map.fromList bindings) `Map.union` env
    rhsEnv = if isrec then env' else env

    instantiateRhs :: TiHeap -> (Name, CoreExpr) -> (TiHeap, (Name, Addr))
    instantiateRhs heap (name, rhs) = (heap', (name, addr))
      where (heap', addr) = instantiate rhs heap rhsEnv

instantiateAndUpdate :: CoreExpr
                     -> Addr -- address to update
                     -> TiHeap
                     -> Map Name Addr
                     -> TiHeap
instantiateAndUpdate (ENum n) upd heap env = Heap.update heap upd (NNum n)
instantiateAndUpdate (EAp e1 e2) upd heap env = Heap.update heap'' upd (NAp a1 a2)
  where
    (heap',  a1) = instantiate e1 heap  env
    (heap'', a2) = instantiate e2 heap' env
instantiateAndUpdate (EVar v) upd heap env = Heap.update heap upd (NInd vAddr)
  where vAddr = Map.findWithDefault (error $ "Unbound indentifier: " ++ v) v env
instantiateAndUpdate (ELet isrec defs body) upd heap env =
    instantiateAndUpdate body upd heap' env'
  where
    (heap', bindings) = mapAccumL instantiateRhs heap defs
    env' = (Map.fromList bindings) `Map.union` env
    rhsEnv = if isrec then env' else env

    instantiateRhs :: TiHeap -> (Name, CoreExpr) -> (TiHeap, (Name, Addr))
    instantiateRhs heap (name, rhs) = (heap', (name, addr))
      where (heap', addr) = instantiate rhs heap rhsEnv
instantiateAndUpdate (EConstr tag arity) upd heap env =
    instantiateAndUpdateConstr tag arity upd heap env

instantiateAndUpdateConstr = error "instantiateAndUpdateConstr: not implemented"

-- Garbage collection
gc :: TiState -> TiState
gc (TiState stack dump heap globals stats) =
    let (heap1, stack')   = markFromStack   heap  stack
        (heap2, dump')    = markFromDump    heap1 dump
        (heap3, globals') = markFromGlobals heap2 globals
        heap4 = scanHeap heap3
    in TiState stack' dump' heap4 globals' stats
markFromStack :: TiHeap -> TiStack -> (TiHeap, TiStack)
markFromStack heap stack = mapAccumL markFrom heap stack

markFromDump :: TiHeap -> TiDump -> (TiHeap, TiDump)
markFromDump heap dump = (heap, dump) -- dump is always empty

markFromGlobals :: TiHeap -> TiGlobals -> (TiHeap, TiGlobals)
markFromGlobals heap globals = foldl (\(heap, globs) name ->
                                          let (heap', addr) = markFrom heap (globs ! name)
                                          in (heap', Map.insert name addr globs)
                                     ) (heap, globals) (Map.keys globals)

markFrom :: TiHeap -> Addr -> (TiHeap, Addr)
markFrom heap addr = case heap *! addr of
    NMarked _ -> (heap, addr)
    NInd directAddr -> markFrom heap directAddr
    (NAp addr1 addr2) -> let heap1 = Heap.update heap addr (NMarked $ NAp addr1' addr2')
                             (heap2, addr1') = markFrom heap1 addr1
                             (heap3, addr2') = markFrom heap2 addr2
                         in (heap3, addr)
    node -> (Heap.update heap addr (NMarked node), addr)

scanHeap :: TiHeap -> TiHeap
scanHeap heap = foldr collect heap (Heap.addresses heap)
  where collect :: Addr -> TiHeap -> TiHeap
        collect addr heap = case heap *! addr of
            NMarked node -> Heap.update heap addr node
            _ -> Heap.free heap addr

-- Formatting results
showResults :: [TiState] -> String
showResults states =
    render $ hcat [ orderedList $ map showState states
                  , showStats . stats $ last states
                  ]

showState :: TiState -> Doc
showState (TiState stack _ heap _ _) = showStack heap stack <> newline

showStack :: TiHeap -> TiStack -> Doc
showStack heap stack = hcat [ str "Stack "
                            , indent $ hcat [str "["
                                            , indent . vcat $ map showItem stack
                                            , newline, str "]"
                                            ]
                            ]
  where showItem :: Addr -> Doc
        showItem addr = hcat [ showFWAddr addr
                             , str ": "
                             , showStkNode heap (heap *! addr)
                             ]

showStkNode :: TiHeap -> Node -> Doc
showStkNode heap (NAp funAddr argAddr) =
    hcat [ str "NAp ", showFWAddr funAddr
         , str " ", showFWAddr argAddr
         , str " (", showNode (heap *! argAddr), str ")"
         ]
showStkNode _ node = showNode node

showNode :: Node -> Doc
showNode (NAp a1 a2) =
    hcat [ str "NAp ", showAddr a1
         , str " ",    showAddr a2
         ]
showNode (NSupercomb name _ _) = str $ "NSupercomb " ++ name
showNode (NNum n) = str "NNum" <+> num n
showNode (NInd a) = str "NInd" <+> num a

showAddr :: Addr -> Doc
showAddr addr = num addr

showFWAddr :: Addr -> Doc -- Show address but forced to min width 4
showFWAddr addr = str (spaces (4 - length s) ++ s)
  where s = show addr
        spaces = flip replicate ' '

showStats :: TiStats -> Doc
showStats stats = hcat [ newline, newline, str "Total number of steps = "
                       , num $ tiStatGetSteps stats
                       ]
